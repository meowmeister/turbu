unit rs_map;
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
   commons, LDB, charset_data, script_interface, transitions, weather,
   rpg_image, addition_sprite;

   procedure teleport(map, x, y: integer);
   procedure teleportVehicle(which: TRpgVehicle; map, x, y: integer);
   procedure teleportEvent(which: TRpgEvent; x, y: integer);
   procedure memorizeLocation(var map, x, y: integer);
   procedure swapEvents(first, second: TRPgEvent);
   procedure rideVehicle;
   function getTerrainID(x, y: word): word;
   function getEventID(x, y: word): word;
   procedure setTransition(const which: TTransitionTypes; const newTransition: TTransitions);
   procedure eraseScreen(whichTransition: TTransitions);
   procedure showScreen(whichTransition: TTransitions);
   procedure setScreenTone(r, g, b, sat: byte; duration: cardinal; wait: boolean);
   procedure flashScreen(r, g, b, power: byte; duration: cardinal; wait: boolean);
   procedure shakeScreen(power, speed: byte; duration: cardinal; wait: boolean);
   procedure lockScreen;
   procedure unlockScreen;
   procedure panScreen(direction: TFacing; distance: word; speed: byte; wait: boolean);
   procedure panScreenTo(x, y: word; speed: byte; wait: boolean);
   procedure returnScreen(speed: byte; wait: boolean);
   procedure setWeather(effect: TWeatherEffects; severity: byte);
   procedure increaseWeather;
   procedure decreaseWeather;
   function newImage(name: string; x, y: integer; zoom, transparency: word; pinned, mask: boolean): TRpgImage;
   procedure setBG(name: string; scrollX, scrollY: shortint; autoX, autoY: boolean);
   procedure showBattleAnim(which: word; target: TRpgCharacter; wait, fullscreen: boolean);
   function prepareRoute(route: string; loop: boolean): word;
   procedure waitUntilMoved;
   procedure stopMoveScripts;

const
   MAX_WEATHER = 10;

implementation
uses
   windows, types, contnrs, math, sysUtils,
   chipset_data, tiles, chipset_graphics, charset_graphics,
   script_engine, locate_files, battle_anims, timing, rpg_anim, move_data;

var
   FDefaultTransitions: array[TTransitionTypes] of TTransitions;

procedure teleport(map, x, y: integer);
var newpoint: TPoint;
begin
   eraseScreen(trnDefault);
   if map = GGameEngine.currentMap.mapID then
   begin
      newpoint := point(x, y);
      if GScriptEngine.onMap(newpoint) then
      begin
         GGameEngine.currentParty.leaveTile;
         GGameEngine.currentParty.location := newpoint;
         GGameEngine.centerOn(x * TILESIZE, y * TILESIZE);
      end;
   end else GGameEngine.changeMaps(map, point(x, y));
   while GGameEngine.state = gs_fading do
      sleep(20);
   showScreen(trnDefault);
end;

procedure teleportEvent(which: TRpgEvent; x, y: integer);
var newpoint: TPoint;
begin
   newpoint := point(x, y);
   if GScriptEngine.onMap(newpoint) then
      which.location := newpoint;
   //end if
end;

procedure teleportVehicle(which: TRpgVehicle; map, x, y: integer);
var newpoint: TPoint;
begin
   if (which.gamesprite = GGameEngine.currentParty) and (map <> GGameEngine.currentMap.mapID) then
      Exit;

   newpoint := point(x, y);
   if GScriptEngine.onMap(newpoint, map) then
   begin
      which.gamesprite.leaveTile;
      which.map := map;
      which.location := newpoint;
   end;
end;

procedure memorizeLocation(var map, x, y: integer);
begin
   if not assigned(GGameEngine.currentParty) then
   begin
      map := 0;
      x := 0;
      y := 0;
   end
   else begin
      map := GGameEngine.currentMap.mapID;
      x := GGameEngine.currentParty.location.x;
      y := GGameEngine.currentParty.location.y;
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
   if GGameEngine.currentParty is THeroSprite then
      (GGameEngine.currentParty as THeroSprite).boardVehicle
   else (GGameEngine.currentParty as TVehicleSprite).state := vs_landing;
end;

function getTerrainID(x, y: word): word;
begin
   if (x > GGameEngine.width) or (y > GGameEngine.height) then
      result := 0
   else result := GGameEngine[lower, x, y].terrain;
end;

function getEventID(x, y: word): word;
var
   dummy: TObjectList;
   i: word;
begin
   result := 0;
   if (x > GGameEngine.width) or (y > GGameEngine.height) then
      Exit;
   dummy := (GGameEngine[lower, x, y] as TLowerTile).event;
   if dummy.count = 0 then
      Exit;

   for I := 0 to dummy.Count - 1 do
   begin
      if (dummy[i] is TEventSprite) or ((dummy[i] is TCharSprite) and not (dummy[i] is TVehicleSprite) and not (dummy[i] is THeroSprite)) then
         result := (dummy[i] as TAdditionSprite).event.id;
      //end if
   end;
end;

procedure setTransition(const which: TTransitionTypes; const newTransition: TTransitions);
begin
   if newTransition = trnDefault then
      Exit;

   FDefaultTransitions[which] := newTransition;
end;

function waitForBlank: boolean;
begin
   result := GGameEngine.blank;
end;

procedure eraseScreen(whichTransition: TTransitions);
begin
   if whichTransition = trnDefault then
      eraseScreen(FDefaultTransitions[trnMapExit])
   else transitions.erase(whichTransition);
   GWaiting := waitForBlank;
end;

function waitForFadeEnd: boolean;
begin
   result := GGameEngine.state <> gs_fading;
end;

procedure showScreen(whichTransition: TTransitions);
begin
   if whichTransition = trnDefault then
      showScreen(FDefaultTransitions[trnMapEnter])
   else transitions.show(whichTransition);
   GWaiting := waitForFadeEnd;
end;

procedure setScreenTone(r, g, b, sat: byte; duration: cardinal; wait: boolean);

   function convert(number: smallint): smallint; inline;
   begin
      number := min(number, 200);
      dec(number, 100);
      result := round(number * 2.55);
   end;

var
   r2, g2, b2: smallint;
begin
   r2 := convert(r);
   g2 := convert(g);
   b2 := convert(b);
   GGameEngine.fadeTo(r2, g2, b2, duration);
   sat := round(min(sat, 100) * 2.55);
   GGameEngine.grayAlpha := 255 - sat;
   if wait then
      TEventThread(GCurrentThread).threadSleep(duration, true);
   //end if
end;

procedure flashScreen(r, g, b, power: byte; duration: cardinal; wait: boolean);
begin
   GGameEngine.flashScreen(r, g, b, power, duration);
   if wait then
   begin
      GDelay := TRpgTimestamp.Create(duration);
      GGameEngine.cutscene := GGameEngine.cutscene + 1;
      GOnHold := true;
   end;
   //end if
end;

procedure shakeScreen(power, speed: byte; duration: cardinal; wait: boolean);
begin
   GGameEngine.shakeScreen(power, speed, duration);
   if wait then
   begin
      GDelay := TRpgTimestamp.Create(duration);
      GGameEngine.cutscene := GGameEngine.cutscene + 1; 
      GOnHold := true;
   end;
   //end if
end;

procedure lockScreen;
begin
   GGameEngine.screenLocked := true;
end;

procedure unlockScreen;
begin
   GGameEngine.screenLocked := false;
end;

procedure panScreen(direction: TFacing; distance: word; speed: byte; wait: boolean);
var
   x, y: integer;
   halfwidth, halfheight: word;
begin
   halfwidth := GGameEngine.Canvas.Width div 2;
   halfheight := GGameEngine.Canvas.Height div 2;
   x := trunc((GGameEngine.worldX + halfwidth) / TILESIZE);
   y := commons.round((GGameEngine.worldY + halfheight) / TILESIZE);
   case direction of
      facing_up: dec(Y, distance);
      facing_right: inc(X, distance);
      facing_down: inc(Y, distance);
      facing_left: dec(x, distance);
   end;
   X := between(x, 0, GGameEngine.width);
   Y := between(y, 0, GGameEngine.height);
   panScreenTo(x, y, speed, wait);
end;

function waitForPanEnd: boolean;
begin
   result := not GGameEngine.displacing;
end;

procedure panScreenTo(x, y: word; speed: byte; wait: boolean);
begin
   GGameEngine.displaceTo(x * TILESIZE, y * TILESIZE);
   GGameEngine.setDispSpeed(speed);
   if wait then
      GWaiting := waitForPanEnd;
   //end if
end;

procedure returnScreen(speed: byte; wait: boolean);
begin
   with GGameEngine.currentParty.location do
      panScreenTo(X, Y, speed, wait);
   GGameEngine.setDispSpeed(speed);
   GGameEngine.returning := true;
   if wait then
      GWaiting := waitForPanEnd;
   //end if
end;

procedure setWeather(effect: TWeatherEffects; severity: byte);
begin
   with GGameEngine.weatherEngine do
   begin
      weatherType := effect;
      severity := min(severity, MAX_WEATHER);
      intensity := severity;
   end;
end;

procedure increaseWeather;
begin
   setWeather(GGameEngine.weatherEngine.weatherType, GGameEngine.weatherEngine.intensity + 1);
end;

procedure decreaseWeather;
begin
   setWeather(GGameEngine.weatherEngine.weatherType, max(GGameEngine.weatherEngine.intensity - 1, 0));
end;

function newImage(name: string; x, y: integer; zoom, transparency: word; pinned, mask: boolean): TRpgImage;
var
   dummy: string;
begin
   if mask then
      dummy := name + 'M'
   else dummy := name + 'NM';
   try
      GGameEngine.loadRpgImage(name, mask);
      result := TRpgImage.Create(GGameEngine, dummy, x, y, zoom, pinned);
      result.opacity := 100 - min(transparency, 100);
   except on E: EParseMessage do //if the file doesn't load
      result := TRpgImage.Create(GGameEngine, '', 0, 0, 0, false);
   end;
end;

procedure setBG(name: string; scrollX, scrollY: shortint; autoX, autoY: boolean);
begin
   GGameEngine.currentMap.setBG(name, scrollX, scrollY, autoX, autoY);
end;

procedure showBattleAnim(which: word; target: TRpgCharacter; wait, fullscreen: boolean);
var
   dummy: TBattleAnim;
   sprite: TAnimSprite;
begin
//   dummy := GDatabase.anim[which];
   if (dummy = nil) or (target = nil) then
      Exit;

   try
      GGameEngine.loadAnim(dummy.filename);
   except on E: EParseMessage do //if the file doesn't load
      Exit;
   end;
   sprite := TAnimSprite.Create(GGameEngine, dummy, target, fullscreen);
   if wait then
      TEventThread(GCurrentThread).threadSleep(sprite.time + 10, true);
end;

function prepareRoute(route: string; loop: boolean): word;
var
   i: smallint;
begin
   if route = '' then
      loop := false;
      
   //is this a copy of a route we've already created?
   for i := 0 to high(GGameEngine.currentMap.routes) do
      if (route = GGameEngine.currentMap.routes[i].base) and (GGameEngine.currentMap.routes[i].loop = loop) then
      begin
         result := i;
         Exit;
      end;

   //if not, create a new route...
   GGameEngine.currentMap.addRoute;
   GGameEngine.currentMap.routes[high(GGameEngine.currentMap.routes)] := TMoveOrder.Create(route, loop);
   result := high(GGameEngine.currentMap.routes);
end;

function allMoved: boolean;
var
   I: Integer;
begin
   result := true;
   i := 0;
   while result and (I < high(GRpgEvents)) do
   begin
      inc(i);
      with GRpgEvents[i] do
         if assigned(base.moveOrder) then
            result := result and (base.moveOrder.looped)
         //end IF
      //end WITH
   end;
end;

procedure waitUntilMoved;
begin
   GWaiting := allMoved;
end;

procedure stopMoveScripts;
var
   I: Integer;
begin
   for I := 1 to high(GRpgEvents) do
      GRpgEvents[i].base.stop;
   for i := 0 to high(GVehicles) do
      GVehicles[i].base.stop;
   GParty.base.stop;
end;

end.
