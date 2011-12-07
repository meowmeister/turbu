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
   Generics.Collections, RTTI,
   rsCompiler, rsExec, rsDefsBackend,
   turbu_map_interface, turbu_map_objects;

type
   TRegisterEnvironmentProc = procedure(compiler: TrsCompiler; importer: TrsTypeImporter);

   TScriptEngine = class
   private
      FCompiler: TrsCompiler;
      FExec: TrsExec;
      FCurrentProgram: TrsProgram;
   public
      constructor Create;
      destructor Destroy; override;
      procedure LoadScript(const script: string);
      procedure LoadLibrary(const script: string);
      procedure LoadEnvironment(proc: TRegisterEnvironmentProc);
      procedure RunScript(const name: string); overload; inline;
      procedure RunScript(const name: string; const args: TArray<TValue>); overload;
   end;

   TMapObjectManager = class
   private
      FMapObjects: TList<TRpgMapObject>;
      FScriptEngine: TScriptEngine;
   public
      constructor Create;
      destructor Destroy; override;

      procedure LoadMap(const map: IRpgMap);

      property ScriptEngine: TScriptEngine read FScriptEngine;
   end;

var
   GScriptEngine: TScriptEngine;
   GMapObjectManager: TMapObjectManager;

implementation
uses
   SysUtils, Classes,
   turbu_defs, turbu_battle_engine;

{ TScriptEngine }

procedure RegisterBattlesC(input: TrsTypeImporter);
begin
   input.ImportType(TypeInfo(TBattleResult));
   input.ImportType(TypeInfo(TBattleResultSet));
   input.ImportType(TypeInfo(TBattleFormation));
   input.ImportFunction('procedure battle(which: integer; const background: string; results: TBattleResultSet; firstStrike: boolean)');
   input.ImportFunction('procedure battleEx(which: integer; const background: string; formation: TBattleFormation; results: TBattleResultSet; bgMode, terrain: integer)');
end;

procedure RegisterCharactersC(input: TrsTypeImporter);
begin
   input.ImportType(TypeInfo(TSlot));
   input.ImportFunction('procedure AddItem(id, quantity: integer);');
   input.ImportFunction('procedure RemoveItem(id, quantity: integer);');
   input.ImportFunction('procedure heroJoin(id: integer);');
   input.ImportFunction('procedure heroLeave(id: integer);');
   input.ImportFunction('procedure addExp(id, number: integer; notify: boolean)');
   input.ImportFunction('procedure RemoveExp(id, number: integer);');
   input.ImportFunction('procedure AddLevels(id, number: integer; showMessage: boolean);');
   input.ImportFunction('procedure RemoveLevels(hero, count: integer);');
end;

procedure RegisterMapsC(input: TrsTypeImporter);
begin
   input.ImportType(TypeInfo(TTransitionTypes));
   input.ImportType(TypeInfo(TTransitions));
   input.ImportType(TypeInfo(TWeatherEffects));
   input.ImportFunction('procedure Teleport(mapID, x, y, facing: integer);');
   input.ImportFunction('procedure memorizeLocation(var map, x, y: integer);');
   input.ImportFunction('procedure rideVehicle;');
   input.ImportFunction('procedure teleportVehicle(which, map, x, y: integer);');
   input.ImportFunction('procedure teleportMapObject(which: TRpgEvent; x, y: integer);');
   input.ImportFunction('procedure swapEvents(first, second: TRpgEvent);');
   input.ImportFunction('function getTerrainID(x, y: integer): integer;');
   input.ImportFunction('function getEventID(x, y: integer): integer;');
   input.ImportFunction('procedure setTransition(const which: TTransitionTypes; const newTransition: TTransitions);');
   input.ImportFunction('procedure eraseScreen(whichTransition: TTransitions);');
   input.ImportFunction('procedure showScreen(whichTransition: TTransitions);');
   input.ImportFunction('procedure tintScreen(r, g, b, sat: integer; duration: integer; wait: boolean);');
   input.ImportFunction('procedure flashScreen(r, g, b, power: integer; duration: integer; wait, continuous: boolean);');
   input.ImportFunction('procedure shakeScreen(power, speed: integer; duration: integer; wait, continuous: boolean);');
   input.ImportFunction('procedure endFlashScreen;');
   input.ImportFunction('procedure endShakeScreen;');
   input.ImportFunction('procedure lockScreen;');
   input.ImportFunction('procedure unlockScreen;');
   input.ImportFunction('procedure panScreen(direction: TFacing; distance: integer; speed: integer; wait: boolean);');
   input.ImportFunction('procedure panScreenTo(x, y: integer; speed: integer; wait: boolean);');
   input.ImportFunction('procedure returnScreen(speed: integer; wait: boolean);');
   input.ImportFunction('procedure setWeather(effect: TWeatherEffects; severity: integer);');
   input.ImportFunction('procedure increaseWeather;');
   input.ImportFunction('procedure decreaseWeather;');
   input.ImportFunction('function newImage(name: string; x, y: integer; zoom, transparency: integer; pinned, mask: boolean): TRpgImage;');
   input.ImportFunction('procedure flashEvent(r, g, b, power: integer; duration: integer; wait: boolean);');
   input.ImportFunction('procedure setBGImage(name: string; scrollX, scrollY: integer; autoX, autoY: boolean);');
   input.ImportFunction('procedure showBattleAnim(which: integer; target: TRpgCharacter; wait, fullscreen: boolean);');
   input.ImportFunction('procedure waitUntilMoved;');
   input.ImportFunction('procedure stopMoveScripts;');
   input.ImportFunction('procedure changeTileset(which: integer)');
   input.ImportFunction('procedure SetEncounterRate(low, high: integer)');
   input.ImportFunction('procedure AddTeleport(mapID, x, y, switchID: integer);');
   input.ImportFunction('procedure DeleteTeleport(mapID, x, y: integer);');
   input.ImportFunction('procedure EnableTeleport(value: boolean);');
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

procedure RegisterMessagesC(input: TrsTypeImporter);
begin
   input.ImportType(TypeInfo(TMboxLocation));
   input.ImportFunction('procedure showMessage(const msg: string)');
   input.ImportFunction('procedure messageOptions(const transparent: boolean; const position: TMboxLocation; const dontHideHero: boolean; const continueEvents: boolean);');
   input.ImportFunction('procedure clearPortrait');
   input.ImportFunction('procedure setPortrait(const filename: string; const index: integer; const rightside, flipped: boolean);');
   input.ImportFunction('function showChoice(input: string; handler: integer): integer;');
   input.ImportFunction('function inputNumber(const digits: integer): integer');
   input.ImportFunction('function inn(messageStyle: integer; cost: integer): boolean;');
   input.ImportFunction('function inputText(const base: string; portrait: integer): string');
   input.ImportFunction('procedure OpenMenu;');
   input.ImportFunction('procedure EnableMenu;');
   input.ImportFunction('procedure DisableMenu;');
   input.ImportFunction('procedure SaveMenu;');
end;

procedure RegisterSettingsC(input: TrsTypeImporter);
begin
   input.ImportType(TypeInfo(TBgmTypes));
   input.ImportType(TypeInfo(TSfxTypes));
   input.ImportFunction('procedure SetSystemMusic(style: TBgmTypes; filename: string; fadeIn, volume, tempo, balance: integer);');
   input.ImportFunction('procedure SetSystemSound(style: TSfxTypes; filename: string; volume, tempo, balance: integer);');
   input.ImportFunction('procedure SetSkin(filename: string);');
end;

constructor TScriptEngine.Create;
begin
   assert(GScriptEngine = nil);
   FCompiler := TrsCompiler.Create;
   FExec := TrsExec.Create;
   GScriptEngine := self;
   FCompiler.RegisterStandardUnit('battles', RegisterBattlesC);
   FCompiler.RegisterStandardUnit('characters', RegisterCharactersC);
   FCompiler.RegisterStandardUnit('maps', RegisterMapsC);
   FCompiler.RegisterStandardUnit('media', RegisterMediaC);
   FCompiler.RegisterStandardUnit('messages', RegisterMessagesC);
   FCompiler.RegisterStandardUnit('settings', RegisterSettingsC);
end;

destructor TScriptEngine.Destroy;
begin
   GScriptEngine := nil;
   FCompiler.Free;
   FCurrentProgram.Free;
   FExec.Free;
   inherited Destroy;
end;

procedure TScriptEngine.LoadEnvironment(proc: TRegisterEnvironmentProc);
var
   importer: TrsTypeImporter;
begin
   importer := TrsTypeImporter.Create(FCompiler, FCompiler.GetUnit('SYSTEM'));
   try
      proc(FCompiler, importer);
   finally
      importer.Free;
   end;
end;

procedure TScriptEngine.LoadLibrary(const script: string);
begin
   FCompiler.CompileUnit(script);
end;

procedure TScriptEngine.LoadScript(const script: string);
begin
   FreeAndNil(FCurrentProgram);
   FCurrentProgram := FCompiler.Compile(script);
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

{ TMapObjectManager }

constructor TMapObjectManager.Create;
begin
   assert(GMapObjectManager = nil);
   GMapObjectManager := self;
   FMapObjects := TList<TRpgMapObject>.Create;
   FScriptEngine := TScriptEngine.Create;
end;

destructor TMapObjectManager.Destroy;
begin
   GMapObjectManager := nil;
   FScriptEngine.Free;
   FMapObjects.Free;
   inherited Destroy;
end;

procedure TMapObjectManager.LoadMap(const map: IRpgMap);
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
end;

end.
