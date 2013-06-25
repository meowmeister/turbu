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
   TThreadWaitEvent = function: boolean;

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

      procedure AddScriptThread(thread: TScriptThread);
      procedure ClearScriptThread(thread: TScriptThread);
      procedure CreateExec;
      procedure InternalLoadEnvironment(compiler: TRsCompiler);
      procedure OnRunLine(Sender: TrsVM; const line: TrsDebugLineInfo);
   public
      constructor Create;
      destructor Destroy; override;
      procedure LoadScript(const script: string; context: TThread = nil);
      procedure LoadLibrary(const script: string);
      procedure LoadEnvironment(proc: TRegisterEnvironmentProc);
      procedure RegisterUnit(const name: string; const comp: TrsCompilerRegisterProc; const exec: TrsExecImportProc);
      procedure RunScript(const name: string); overload; inline;
      procedure RunScript(const name: string; const args: TArray<TValue>); overload;
      procedure RunObjectScript(obj: TRpgMapObject; page: integer); overload;
      procedure KillAll;
      procedure threadSleep(time: integer; block: boolean = false);
      procedure SetWaiting(value: TThreadWaitEvent);
   end;

   TMapObjectManager = class
   private
      FMapObjects: TList<TRpgMapObject>;
      FScriptEngine: TScriptEngine;
      FPlaylist: TList<TRpgEventPage>;
      FOnUpdate: TProc;
   public
      constructor Create;
      destructor Destroy; override;

      procedure LoadMap(const map: IRpgMap; context: TThread = nil);
      procedure Tick;
      procedure RunPageScript(page: TRpgEventPage);

      property ScriptEngine: TScriptEngine read FScriptEngine;
      property OnUpdate: TProc read FOnUpdate write FOnUpdate;
   end;

var
   GScriptEngine: TScriptEngine;
   GMapObjectManager: TMapObjectManager;

implementation
uses
   Math, Forms,
   turbu_defs, turbu_battle_engine, logs,
   rs_media,
   SDL;

threadvar
   GOnHold: boolean;

procedure RegisterBattlesC(input: TrsTypeImporter);
begin
   input.ImportType(TypeInfo(TBattleResult));
   input.ImportType(TypeInfo(TBattleResultSet));
   input.ImportType(TypeInfo(TBattleFormation));
   input.ImportFunction('procedure battle(which: integer; const background: string; results: TBattleResultSet; firstStrike: boolean)');
   input.ImportFunction('procedure battleEx(which: integer; const background: string; formation: TBattleFormation; results: TBattleResultSet; bgMode, terrain: integer)');
end;

procedure RegisterMediaC(input: TrsTypeImporter);
begin
   input.ImportFunction('procedure playMusic(name: string; fadeIn, volume, tempo, balance: integer);');
   input.ImportFunction('procedure fadeOutMusic(time: integer);');
   input.ImportFunction('procedure memorizeBgm;');
   input.ImportFunction('procedure playMemorizedBgm;');
   input.ImportFunction('procedure playSound(name: string; volume, tempo, balance: integer);');
   input.ImportFunction('procedure playMovie(name: string; posX, posY, width, height: integer);');
end;

procedure RegisterSettingsC(input: TrsTypeImporter);
begin
   input.ImportType(TypeInfo(TBgmTypes));
   input.ImportType(TypeInfo(TSfxTypes));
   input.ImportFunction('procedure SetSystemMusic(style: TBgmTypes; filename: string; fadeIn, volume, tempo, balance: integer);');
   input.ImportFunction('procedure SetSystemSound(style: TSfxTypes; filename: string; volume, tempo, balance: integer);');
   input.ImportFunction('procedure SetSkin(filename: string);');
end;

procedure RegisterBattlesE(RegisterFunction: TExecImportCall; RegisterArrayProp: TArrayPropImport);
begin
   RegisterFunction('battle', nil);
   RegisterFunction('battleEx', nil);
end;

procedure RegisterMediaE(RegisterFunction: TExecImportCall; RegisterArrayProp: TArrayPropImport);
begin
   RegisterFunction('playMusic', @rs_media.playMusic);
   RegisterFunction('fadeOutMusic', @rs_media.FadeOutMusic);
   RegisterFunction('memorizeBgm', nil);
   RegisterFunction('playMemorizedBgm', nil);
   RegisterFunction('playSound', @rs_Media.playSound);
   RegisterFunction('playMovie', nil);
end;

procedure RegisterSettingsE(RegisterFunction: TExecImportCall; RegisterArrayProp: TArrayPropImport);
begin
   RegisterFunction('SetSystemMusic', nil);
   RegisterFunction('SetSystemSound', nil);
   RegisterFunction('SetSkin', nil);
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

   FCompiler.RegisterStandardUnit('battles', RegisterBattlesC);
   FCompiler.RegisterStandardUnit('media', RegisterMediaC);
   FCompiler.RegisterStandardUnit('settings', RegisterSettingsC);

end;

destructor TScriptEngine.Destroy;
begin
   GScriptEngine := nil;
   FThreads.Free;
   FThreadLock.Free;
   FCompiler.Free;
   FCurrentProgram.Free;
   FExec.Free;
   inherited Destroy;
end;

procedure TScriptEngine.CreateExec;
begin
   FExec := TrsExec.Create;
   FExec.RegisterStandardUnit('battles', RegisterBattlesE);
   FExec.RegisterStandardUnit('media', RegisterMediaE);
   FExec.RegisterStandardUnit('settings', RegisterSettingsE);
   FExec.OnLine := self.OnRunLine;
end;

procedure TScriptEngine.OnRunLine(Sender: TrsVM; const line: TrsDebugLineInfo);
var
   st: TScriptThread;
begin
   st := TThread.CurrentThread as TScriptThread;
   st.scriptOnLine(sender);
end;

procedure TScriptEngine.KillAll;
var
   curr: TThread;
   thread: TScriptThread;
   done: boolean;
begin
   curr := TThread.CurrentThread;
   if not (curr is TScriptThread) then
      curr := nil;
   FThreadLock.Enter;
   for thread in FThreads do
      if thread <> curr then
         thread.Terminate;
   FThreadLock.Leave;

   repeat
      sleep(10);
      FThreadLock.Enter;
      done := ((curr = nil) and (FThreads.Count = 0)) or ((FThreads.Count = 1) and (FThreads[0] = curr));
      FThreadLock.Leave;
      if TThread.CurrentThread.ThreadID = MainThreadID then
         CheckSynchronize();
   until done;
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
   logs.logText(script);
   if context is TScriptThread then
   begin
      TScriptThread(context).FOwnedExec := FExec;
      TScriptThread(context).FOwnedProgram := FCurrentProgram;
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

procedure TScriptEngine.RunObjectScript(obj: TRpgMapObject; page: integer);
var
   context: TScriptThread;
   lPage: TRpgEventPage;
begin
   context := TThread.CurrentThread as TScriptThread;
   lPage := obj.pages[page];
   assert(assigned(context.FPage));
   context.PushPage(lPage);
   try
      RunScript(lPage.scriptName);
   finally
      context.PopPage;
   end;
end;

procedure TScriptEngine.RegisterUnit(const name: string;
  const comp: TrsCompilerRegisterProc; const exec: TrsExecImportProc);
begin
   FCompiler.RegisterStandardUnit(name, comp);
   FExec.RegisterStandardUnit(name, exec);
   SetLength(FImports, length(FImports) + 1);
   FImports[high(FImports)] := TPair<string, TrsExecImportProc>.Create(name, exec);
end;

{$O-}
procedure TScriptEngine.SetWaiting(value: TThreadWaitEvent);
var
   st: TScriptThread;
begin
   st := TThread.CurrentThread as TScriptThread;
   st.FWaiting := value;
end;

procedure TScriptEngine.threadSleep(time: integer; block: boolean = false);
var
   st: TScriptThread;
begin
   st := TThread.CurrentThread as TScriptThread;
   st.FDelay := TRpgTimestamp.Create(time);
{   if block then
      GGameEngine.cutscene := GGameEngine.cutscene + 1;}
   st.InternalThreadSleep;
{   if block then
      GGameEngine.cutscene := GGameEngine.cutscene - 1;}
end;

{ TMapObjectManager }

constructor TMapObjectManager.Create;
begin
   assert(GMapObjectManager = nil);
   GMapObjectManager := self;
   FMapObjects := TList<TRpgMapObject>.Create;
   FScriptEngine := TScriptEngine.Create;
   FPlaylist := TList<TRpgEventPage>.Create;
end;

destructor TMapObjectManager.Destroy;
begin
   GMapObjectManager := nil;
   FPlaylist.Free;
   FScriptEngine.Free;
   FMapObjects.Free;
   inherited Destroy;
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
   page.parent.playing := true;
   thread := TScriptThread.Create(page, FScriptEngine);
   thread.FreeOnTerminate := true;
   {$IFDEF DEBUG}
   TThread.NameThreadForDebugging(AnsiString(format('Script thread: %s', [page.ScriptName])), thread.ThreadID);
   {$ENDIF}
   thread.Start;
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
   if assigned(FOnUpdate) then
      FOnUpdate;
   for page in FPlaylist do
      RunPageScript(page);
end;

{ TScriptThread }

constructor TScriptThread.Create(page: TRpgEventPage; parent: TScriptEngine);
begin
   inherited Create(true);
   FPage := page;
   FParent := parent;
   parent.AddScriptThread(self);
end;

function TScriptThread.CurrentObject: TRpgMapObject;
begin
   result := FPage.parent;
end;

destructor TScriptThread.Destroy;
begin
   FOwnedExec.Free;
   FOwnedProgram.Free;
   FParent.ClearScriptThread(self);
   FPages.Free;
   inherited Destroy;
end;

procedure TScriptThread.Execute;
begin
   try
      if FPage.scriptName <> '' then
      begin
         NameThreadForDebugging('TURBU Script Thread');
         FParent.RunScript(FPage.scriptName);
      end;
   finally
      FPage.parent.playing := false;
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
      until (FWaiting() = true) or (self.Terminated);
      FWaiting := nil;
   end
   else if assigned(FDelay) then
      if (FDelay.timeRemaining > 0) then
         threadSleep(sender)
      else
      begin
         freeAndNil(FDelay);
         if GOnHold then
         begin
//            GGameEngine.cutscene := GGameEngine.cutscene - 1;
            GOnHold := false;
         end;
      end;
   if Self.Terminated then
      Abort;
end;

end.
