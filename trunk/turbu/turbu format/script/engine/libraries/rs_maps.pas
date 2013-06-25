unit rs_maps;
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
   turbu_mapchars, turbu_defs, turbu_2k_images, turbu_script_engine;

   procedure Teleport(mapID, x, y, facing: integer);
   procedure teleportVehicle(which: TRpgVehicle; map, x, y: integer);
   procedure teleportEvent(which: TRpgEvent; x, y: integer);
   procedure memorizeLocation(var map, x, y: integer);
   procedure swapEvents(first, second: TRpgEvent);
   procedure rideVehicle;
   function getTerrainID(x, y: integer): integer;
   function getEventID(x, y: integer): integer;
   procedure setTransition(const which: TTransitionTypes; const newTransition: TTransitions);
   procedure eraseScreen(whichTransition: TTransitions);
   procedure showScreen(whichTransition: TTransitions);
   procedure tintScreen(r, g, b, sat: integer; duration: integer; wait: boolean);
   procedure flashScreen(r, g, b, power: integer; duration: integer; wait, continuous: boolean);
   procedure shakeScreen(power, speed: integer; duration: integer; wait, continuous: boolean);
   procedure lockScreen;
   procedure unlockScreen;
   procedure panScreen(direction: TFacing; distance: integer; speed: integer; wait: boolean);
   procedure panScreenTo(x, y: integer; speed: integer; wait: boolean);
   procedure returnScreen(speed: integer; wait: boolean);
   procedure setWeather(effect: TWeatherEffects; severity: integer);
   procedure increaseWeather;
   procedure decreaseWeather;
   function newImage(name: string; x, y: integer; zoom, transparency: integer; pinned, mask: boolean): TRpgImage;
   procedure setBG(name: string; scrollX, scrollY: integer; autoX, autoY: boolean);
   procedure showBattleAnim(which: integer; target: TRpgCharacter; wait, fullscreen: boolean);
   procedure waitUntilMoved;
   procedure stopMoveScripts;

   procedure RegisterScriptUnit(engine: TScriptEngine);

const
   MAX_WEATHER = 10;

implementation
uses
   Windows, SysUtils, types, math, SyncObjs,
   rsExec, rsCompiler,
   commons, turbu_2k_environment, turbu_2k_sprite_engine, turbu_constants,
   turbu_database, turbu_2k_map_engine, turbu_animations, turbu_2k_animations,
   turbu_pathing, turbu_2k_char_sprites, turbu_map_sprites;

var
   FDefaultTransitions: array[TTransitionTypes] of TTransitions;

procedure Teleport(mapID, x, y, facing: integer);
var newpoint: TPoint;
begin
   eraseScreen(trn_Default);

   if mapID = GSpriteEngine.MapID then
   begin
      newpoint := point(x, y);
      if GSpriteEngine.onMap(newpoint) then
      begin
         GEnvironment.Party.Sprite.leaveTile;
         GEnvironment.Party.Sprite.location := newpoint;
         GSpriteEngine.centerOn(x, y);
      end;
   end else GGameEngine.changeMaps(mapID, point(x, y));
   showScreen(trn_Default);
end;

procedure teleportEvent(which: TRpgEvent; x, y: integer);
var newpoint: TPoint;
begin
   newpoint := point(x, y);
   if GSpriteEngine.onMap(newpoint) then
      which.location := newpoint;
end;

procedure teleportVehicle(which: TRpgVehicle; map, x, y: integer);
var newpoint: TPoint;
begin
   if (which.gamesprite = GEnvironment.Party.Sprite) and (map <> GSpriteEngine.mapID) then
      Exit;

   newpoint := point(x, y);
   if GSpriteEngine.onMap(newpoint) then
   begin
      which.gamesprite.leaveTile;
      which.map := map;
      which.location := newpoint;
   end;
end;

procedure memorizeLocation(var map, x, y: integer);
begin
   if not assigned(GEnvironment.Party) then
   begin
      map := 0;
      x := 0;
      y := 0;
   end
   else begin
      map := GGameEngine.currentMap.mapID;
      x := GEnvironment.Party.Sprite.location.x;
      y := GEnvironment.Party.Sprite.location.y;
   end;
end;

procedure swapEvents(first, second: TRPgEvent);
var swapper: TPoint;
begin
   first.base.leaveTile;
   second.base.leaveTile;
   swapper := first.location;
   first.location := second.location;
   second.location := swapper;
end;

procedure rideVehicle;
begin
   if GEnvironment.Party.Sprite is THeroSprite then
      (GEnvironment.Party.Sprite as THeroSprite).boardVehicle
   else (GEnvironment.Party.Sprite as TVehicleSprite).state := vs_landing;
end;

function getTerrainID(x, y: integer): integer;
begin
   if (x > GGameEngine.CurrentMap.width) or (y > GGameEngine.CurrentMap.height) then
      result := 0
   else result := GGameEngine.CurrentMap.GetTile(x, y, 0).terrain;
end;

function getEventID(x, y: integer): integer;
var
   events: TArray<TMapSprite>;
   i: integer;
begin
   result := 0;
   if (x > GGameEngine.CurrentMap.width) or (y > GGameEngine.CurrentMap.height) then
      Exit;
   events := GGameEngine.CurrentMap.GetTile(x, y, 0).event;

   for I := 0 to high(events) do
   begin
      if (events[i] is TEventSprite) or ((events[i] is TCharSprite) and not (events[i] is TVehicleSprite) and not (events[i] is THeroSprite)) then
         result := events[i].event.id;
   end;
end;

procedure setTransition(const which: TTransitionTypes; const newTransition: TTransitions);
begin
{$MESSAGE WARN 'Commented out code in live unit'}
{   if newTransition = trnDefault then
      Exit;

   FDefaultTransitions[which] := newTransition;}
end;

function waitForBlank: boolean;
begin
   result := GGameEngine.CurrentMap.blank;
end;

procedure eraseScreen(whichTransition: TTransitions);
begin
{$MESSAGE WARN 'Commented out code in live unit'}
{   if whichTransition = trnDefault then
      eraseScreen(FDefaultTransitions[trnMapExit])
   else transitions.erase(whichTransition);
   GWaiting := waitForBlank;}
end;

function waitForFadeEnd: boolean;
begin
{$MESSAGE WARN 'Commented out code in live unit'}
//   result := GGameEngine.state <> gs_fading;
result := true;
end;

procedure showScreen(whichTransition: TTransitions);
begin
{$MESSAGE WARN 'Commented out code in live unit'}
{   if whichTransition = trnDefault then
      showScreen(FDefaultTransitions[trnMapEnter])
   else transitions.show(whichTransition);
   GWaiting := waitForFadeEnd;}
end;

procedure tintScreen(r, g, b, sat: integer; duration: integer; wait: boolean);

   function convert(number: integer): integer; inline;
   begin
      result := round(clamp(number, 0, 200) * 2.55);
   end;

var
   r2, g2, b2, s2: integer;
begin
   r2 := convert(r);
   g2 := convert(g);
   b2 := convert(b);
   s2 := convert(sat);
   GSpriteEngine.fadeTo(r2, g2, b2, s2, duration);
   if wait then
      GScriptEngine.threadSleep(duration * 100, true);
end;

procedure flashScreen(r, g, b, power: integer; duration: integer; wait, continuous: boolean);
begin
   GSpriteEngine.flashScreen(r, g, b, power, duration);
   if wait then
      GScriptEngine.threadSleep(duration * 100, true);
end;

procedure shakeScreen(power, speed: integer; duration: integer; wait, continuous: boolean);
begin
   GSpriteEngine.shakeScreen(power, speed, duration * 100);
   if wait then
      GScriptEngine.threadSleep(duration * 100, true);
end;

procedure lockScreen;
begin
{$MESSAGE WARN 'Commented out code in live unit'}
//   GGameEngine.screenLocked := true;
end;

procedure unlockScreen;
begin
{$MESSAGE WARN 'Commented out code in live unit'}
//   GGameEngine.screenLocked := false;
end;

procedure panScreen(direction: TFacing; distance: integer; speed: integer; wait: boolean);
var
   x, y: integer;
   halfwidth, halfheight: integer;
begin
   halfwidth := GSpriteEngine.Canvas.Width div 2;
   halfheight := GSpriteEngine.Canvas.Height div 2;
   x := trunc((GSpriteEngine.worldX + halfwidth) / TILE_SIZE.x);
   y := commons.round((GSpriteEngine.worldY + halfheight) / TILE_SIZE.y);
   case direction of
      facing_up: dec(Y, distance);
      facing_right: inc(X, distance);
      facing_down: inc(Y, distance);
      facing_left: dec(x, distance);
   end;
   X := clamp(x, 0, GSpriteEngine.width);
   Y := clamp(y, 0, GSpriteEngine.height);
   panScreenTo(x, y, speed, wait);
end;

function waitForPanEnd: boolean;
begin
   result := not GSpriteEngine.displacing;
end;

procedure panScreenTo(x, y: integer; speed: integer; wait: boolean);
begin
   GSpriteEngine.displaceTo(x * TILE_SIZE.x, y * TILE_SIZE.y);
   GSpriteEngine.setDispSpeed(speed);
   if wait then
      GScriptEngine.SetWaiting(waitForPanEnd);
end;

procedure returnScreen(speed: integer; wait: boolean);
begin
   panScreenTo(GEnvironment.Party.xPos, GEnvironment.Party.yPos, speed, wait);
   GSpriteEngine.setDispSpeed(speed);
   GSpriteEngine.returning := true;
   if wait then
      GScriptEngine.SetWaiting(waitForPanEnd);
end;

procedure setWeather(effect: TWeatherEffects; severity: integer);
begin
{$MESSAGE WARN 'Commented out code in live unit'}
{   with GGameEngine.weatherEngine do
   begin
      weatherType := effect;
      severity := min(severity, MAX_WEATHER);
      intensity := severity;
   end;}
end;

procedure increaseWeather;
begin
{$MESSAGE WARN 'Commented out code in live unit'}
//   setWeather(GGameEngine.weatherEngine.weatherType, GGameEngine.weatherEngine.intensity + 1);
end;

procedure decreaseWeather;
begin
{$MESSAGE WARN 'Commented out code in live unit'}
//   setWeather(GGameEngine.weatherEngine.weatherType, max(GGameEngine.weatherEngine.intensity - 1, 0));
end;

function newImage(name: string; x, y: integer; zoom, transparency: integer; pinned, mask: boolean): TRpgImage;
var
   image: TRpgImage;
begin
   runThreadsafe(
   procedure
      begin
         try
            GGameEngine.loadRpgImage(name, mask);
            image := TRpgImage.Create(GGameEngine.ImageEngine, name, x, y, zoom, pinned, mask);
            image.opacity := 100 - min(transparency, 100);
         except //if the file doesn't load
            image := TRpgImage.Create(GSpriteEngine, '', 0, 0, 0, false, false);
         end;
      end, true);
   result := image;
end;

procedure setBG(name: string; scrollX, scrollY: integer; autoX, autoY: boolean);
begin
{$MESSAGE WARN 'Commented out code in live unit'}
//   GGameEngine.currentMap.setBG(name, scrollX, scrollY, autoX, autoY);
end;

procedure LoadAnim(const filename: string);
begin
   commons.runThreadsafe(
      procedure begin
      GSpriteEngine.Images.EnsureImage(format('animation\%s.png', [filename]), 'Anim '+ filename) end,
      true);
end;

threadvar
   LSignal: TSimpleEvent;

function AnimWait: boolean;
begin
   assert(assigned(LSignal));
   result := LSignal.WaitFor(0) = wrSignaled;
   if result then
      FreeAndNil(LSignal);
end;

procedure showBattleAnim(which: integer; target: TRpgCharacter; wait, fullscreen: boolean);
var
   dummy: TAnimTemplate;
begin
   if target = nil then
      Exit;
   commons.runThreadsafe(
      procedure begin dummy := GDatabase.anim[which] end, true);
   if (dummy = nil) then
      Exit;

   try
      loadAnim(dummy.filename);
   except //if the file doesn't load
      Exit;
   end;
   if wait then
   begin
      assert(LSignal = nil);
      LSignal := TSimpleEvent.Create;
   end;
   TAnimSprite.Create(GSpriteEngine, dummy, target, fullscreen, LSignal);
   if wait then
      GScriptEngine.SetWaiting(AnimWait);
end;

function allMoved: boolean;
var
   I: Integer;
   obj: TRpgEvent;
   partyMove: TPath;
begin
   result := true;
   partyMove := GEnvironment.Party.Sprite.moveOrder;
   if assigned(partyMove) and not (partYMove.looped) then
      result := false;
   i := 0;
   while result and (I <= GEnvironment.MapObjectCount) do
   begin
      inc(i);
      obj := GEnvironment.MapObject[i];
         if assigned(obj) and assigned(obj.base.moveOrder) then
            result := obj.base.moveOrder.looped
   end;
end;

procedure waitUntilMoved;
begin
   GScriptEngine.SetWaiting(allMoved);
end;

procedure stopMoveScripts;
var
   I: Integer;
begin
   for I := 1 to GEnvironment.MapObjectCount - 1 do
      GEnvironment.MapObject[i].base.stop;
{   for i := 0 to high(GVehicles) do
      GVehicles[i].base.stop;}
{$MESSAGE WARN 'Commented out code in live unit'}
   GEnvironment.Party.base.stop;
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
   input.ImportFunction('procedure returnScreen(speed: integer; wait: boolean);');
   input.ImportFunction('procedure setWeather(effect: TWeatherEffects; severity: integer);');
   input.ImportFunction('procedure increaseWeather;');
   input.ImportFunction('procedure decreaseWeather;');
   input.ImportFunction('function newImage(name: string; x, y: integer; zoom, transparency: integer; pinned, mask: boolean): TRpgImage;');
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

procedure RegisterMapsE(RegisterFunction: TExecImportCall; RegisterArrayProp: TArrayPropImport);
begin
   RegisterFunction('Teleport', @teleport);
   RegisterFunction('memorizeLocation', nil);
   RegisterFunction('rideVehicle', nil);
   RegisterFunction('teleportVehicle', nil);
   RegisterFunction('teleportMapObject', nil);
   RegisterFunction('swapEvents', nil);
   RegisterFunction('getTerrainID', nil);
   RegisterFunction('getEventID', nil);
   RegisterFunction('setTransition', nil);
   RegisterFunction('eraseScreen', @eraseScreen);
   RegisterFunction('showScreen', @showScreen);
   RegisterFunction('tintScreen', @tintScreen);
   RegisterFunction('flashScreen', @flashScreen);
   RegisterFunction('shakeScreen', @shakeScreen);
   RegisterFunction('endFlashScreen', nil);
   RegisterFunction('endShakeScreen', nil);
   RegisterFunction('lockScreen', nil);
   RegisterFunction('unlockScreen', nil);
   RegisterFunction('panScreen', @PanScreen);
   RegisterFunction('returnScreen', @ReturnScreen);
   RegisterFunction('setWeather', nil);
   RegisterFunction('increaseWeather', nil);
   RegisterFunction('decreaseWeather', nil);
   RegisterFunction('newImage', @newImage);
   RegisterFunction('setBGImage', nil);
   RegisterFunction('showBattleAnim', @ShowBattleAnim);
   RegisterFunction('waitUntilMoved', @waitUntilMoved);
   RegisterFunction('stopMoveScripts', nil);
   RegisterFunction('changeTileset', nil);
   RegisterFunction('SetEncounterRate', nil);
   RegisterFunction('AddTeleport', nil);
   RegisterFunction('DeleteTeleport', nil);
   RegisterFunction('EnableTeleport', nil);
end;

procedure RegisterScriptUnit(engine: TScriptEngine);
begin
   engine.RegisterUnit('maps', RegisterMapsC, RegisterMapsE);
end;

end.
