unit addition_sprite;
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

{$I ..\..\..\..\unfinished.inc}

interface
uses
   types,
   {tiles,} timing, events, charset_data, move_data, commons,
   {asphyreSprite} SDL_sprite, SG_defs;

type
   TAdditionSprite = class;

   TRpgCharacter = class(TObject)
   private
      function getScreenX: integer; inline;
      function getScreenY: integer; inline;
      function getScreenXP: integer; inline;
      function getScreenYP: integer; inline;
   protected
      procedure doFlash(r, g, b, power: byte; time: cardinal); virtual; abstract;
      function getX: word; virtual; abstract;
      function getY: word; virtual; abstract;
      function getBase: TAdditionSprite; virtual; abstract;
      function getTranslucency: byte; virtual;
      procedure setTranslucency(const Value: byte); virtual;
   public
      procedure flash(r, g, b, power: byte; time: cardinal; wait: boolean);
      procedure move(frequency: byte; skip: boolean; route: word);
      procedure changeSprite(filename: string; index: byte); virtual; abstract;

      property screenX: integer read getScreenX;
      property screenY: integer read getScreenY;
      property screenXP: integer read getScreenXP;
      property screenYP: integer read getScreenYP;
      property base: TAdditionSprite read getBase;
      property translucency: byte read getTranslucency write setTranslucency;
   end;

   TAdditionSprite = class(TObject)
   private
      FMoveQueue: TMoveOrder;
      FMoveAssignment: TMoveOrder;
      FOrder: TMoveRecord;
      FDirLocked: boolean;
      FMoveReversed: boolean;
      FMoveOpen: boolean;
      FFacing: TFacing;
      FInitialized: boolean;
      FJumping: boolean;
      FLastJumpOp: shortint;
      FJumpTarget: TSgPoint;
      FTransparencyFactor: byte;
      FPaused: boolean;

      function tryMove(where: TFacing): boolean; inline;
      function tryMoveDiagonal(one, two: TFacing): boolean; inline;
      function towardsHero: TFacing; inline;
      function canJump(which: TMoveOrder): boolean;
      function isDirLocked: boolean;
      procedure startMoveTo(target: TSgPoint); overload;
      procedure setMoveOrder(const Value: TMoveOrder);
      procedure beginJump;
      procedure endJump;
      procedure incTransparencyFactor;
      procedure decTransparencyFactor;
      function getHasPage: boolean; inline;
      function getAnimFix: boolean;

      property dirLocked: boolean read isDirLocked write FDirLocked;
   protected
      FCharacter: TRpgCharacter;
      FLocation: TSgPoint;
      FTarget: TSgPoint;
      FMoveDir: TFacing;
      FTiles: array [TCharTiles] of TSprite;
      FMoving: byte;
      FEngine: TSpriteEngine;
      FMoveRate: byte;
      FMoveFreq: byte;
      FSlipThrough: boolean;
      FJumpAnimateOverride: boolean;
      FAnimFix: boolean;
      FEvent: TEvent;
      FVisible: boolean;
      FWhichFrame: smallint;
      FPause: TRpgTimestamp;
      FMoveTime: TRpgTimestamp;
      FCanSkip: boolean;
      FUnderConstruction: boolean;

      procedure setFacing(data: TFacing); virtual;
      procedure setVisible(const Value: boolean);
      function getBaseTile: TSprite; inline;
      procedure setLocation(data: TSgPoint); virtual;
      function canMoveForward: boolean; virtual;
      function canMoveDiagonal(one, two: TFacing; out destination: TSgPoint): boolean;
      function onMap: boolean; inline;
      procedure setMovement(direction: TFacing);
      function doMove(which: TMoveOrder): boolean; virtual;
      function getCanSkip: boolean; virtual;
      procedure setTranslucency(const value: byte); virtual;

      property animFix: boolean read getAnimFix write FAnimFix;
      property moveQueue: TMoveOrder read FMoveQueue;
      property moveAssign: TMoveOrder read FMoveAssignment;
   public
      constructor create(baseEvent: TEvent; const AParent: TSpriteEngine; char: TRpgCharacter); reintroduce; virtual;
      destructor Destroy; override;
      function move(whichDir: TFacing): boolean; virtual;
      function moveDiag(one, two: TFacing): boolean;
      procedure leaveTile;
      procedure place; virtual;
      procedure updatePage(data: TEventPage); virtual; abstract;
      procedure nuke(removeself: boolean = false);
      function inFront: TSgPoint;
//      function inFrontTile: TTile; inline;
      procedure flash(r, g, b, power: byte; time: cardinal);
      procedure moveTick;
      procedure update(filename: string; index: byte; transparent: boolean); virtual; abstract;
      procedure pause;
      procedure resume;
      procedure stop;

      property moveRate: byte read FMoveRate write FMoveRate;
      property moveFreq: byte read FMoveFreq write FMoveFreq;
      property location: TSgPoint read FLocation write setLocation;
      property event: TEvent read FEvent write FEvent;
      property baseTile: TSprite read getBaseTile;
      property visible: boolean read FVisible write setVisible;
      property facing: TFacing read FFacing write setFacing;
      property moveOrder: TMoveOrder read FMoveAssignment write setMoveOrder;
      property canSkip: boolean read getCanSkip write FCanSkip;
      property character: TRpgCharacter read FCharacter write FCharacter;
      property translucency: byte read FTransparencyFactor write setTranslucency;
      property hasPage: boolean read getHasPage;
   end;

const
   BASE_MOVE_DELAY = 133;
   MOVE_DELAY: array[1..6] of integer = (BASE_MOVE_DELAY * 8, BASE_MOVE_DELAY * 4, BASE_MOVE_DELAY * 2, BASE_MOVE_DELAY, BASE_MOVE_DELAY div 2, BASE_MOVE_DELAY div 4);
   WIDTH_BIAS = 4;
//   BASE_FOOTSTEP_CONSTANT = 6;
   FOOTSTEP_CONSTANT: array[1..6] of integer = (11, 10, 8, 6, 5, 5); //yay for fudge factors!
   MAX_TRANSPARENCY = 7;
   TRANSPARENCY_STEP = 255 div (MAX_TRANSPARENCY + 1);

implementation
uses
   windows, sysUtils, classes, math,
   {chipset_graphics, charset_graphics,} chipset_data, {script_engine,} rm_sound;

const OP_CLEAR = $C0; //arbitrary value

{TAdditionSprite}

constructor TAdditionSprite.create(baseEvent: TEvent; const AParent: TSpriteEngine; char: TRpgCharacter);
begin
   inherited Create;
   FCharacter := char;
{   FEngine := AParent as TGameMap;
   FMoveRate := 4;
   FEvent := baseEvent;
   FWhichFrame := -1;
   FOrder.opcode := OP_CLEAR;
   if self.hasPage then
   begin
      FMoveRate := FEvent.lastCurrentPage.moveSpeed;
      FMoveFreq := FEvent.lastCurrentPage.moveFrequency;
      FCanSkip := true;
      case FEvent.lastCurrentPage.moveType of
         still, cycle_ud, cycle_lr: ;
         randomMove: FMoveQueue := TMoveOrder.Create('8', true);
         chase_hero: FMoveQueue := TMoveOrder.Create('9', true);
         flee_hero: FMoveQueue := TMoveOrder.Create('10', true);
         by_route:
         begin
            assert(assigned(FEvent.lastCurrentPage.moveBlock.moveBlock));
            FMoveQueue := TMoveOrder.Assign(FEvent.lastCurrentPage.moveBlock.moveBlock);
            FCanSkip := FEvent.lastCurrentPage.moveBlock.ignore;
         end;
         else assert(false);
      end;
   end;}
end;

procedure TAdditionSprite.decTransparencyFactor;
begin
   self.translucency := max(FTransparencyFactor - 1, 0);
end;

procedure TAdditionSprite.incTransparencyFactor;
begin
   self.translucency := min(FTransparencyFactor + 1, MAX_TRANSPARENCY);
end;

function TAdditionSprite.inFront: TSgPoint;
begin
   case FFacing of
      facing_up: result := point(FLocation.x, FLocation.y - 1);
      facing_right: result := point(FLocation.x + 1, FLocation.y);
      facing_down: result := point(FLocation.x, FLocation.y + 1);
      facing_left: result := point(FLocation.x - 1, FLocation.y);
   end;
end;

{function TAdditionSprite.inFrontTile: TTile;
var dummy: TSgPoint;
begin
   dummy := self.inFront;
//   result := TGameMap(FEngine)[lower, dummy.x, dummy.y];
end;}

procedure TAdditionSprite.leaveTile;
begin
{   GEventLock.enter;
   while TLowerTile(TGameMap(FEngine)[lower, location.x, location.y]).event.indexOf(self) <> -1 do
      TLowerTile(TGameMap(FEngine)[lower, location.x, location.y]).event.Remove(self);
   TLowerTile(TGameMap(FEngine)[lower, location.x, location.y]).event.Pack;
   GEventLock.leave;}
end;

function TAdditionSprite.tryMove(where: TFacing): boolean;
begin
   FMoveOpen := self.move(where);
   result := FMoveOpen or canSkip;
end;

function TAdditionSprite.tryMoveDiagonal(one, two: TFacing): boolean;
begin
   result := self.moveDiag(one, two) or canSkip;
end;

procedure TAdditionSprite.beginJump;
var midpoint: TSgPoint;
begin
   self.leaveTile;
   FJumping := true;
   FJumpAnimateOverride := true;
{   midpoint := ((FJumpTarget - FLocation) * TILESIZE) / 2;
   dec(midpoint.Y, TILESIZE div 2);
   FTarget := (FLocation * TILESIZE) + midpoint;
   dec(FTarget.x, WIDTH_BIAS);
   if not self.dirLocked and (FLocation <> FJumpTarget) then
      self.facing := towards(FLocation, FJumpTarget);
   FLocation := FJumpTarget;
   assert(not assigned(FMoveTime));
   TLowerTile(TGameMap(FEngine)[lower, FLocation.x, FLocation.y]).event.Add(self);
   FMoveTime := TRpgTimestamp.Create(commons.round(MOVE_DELAY[FMoveRate] / 2.5));}
end;

procedure TAdditionSprite.endJump;
begin
{   FTarget := FLocation * TILESIZE;
   dec(FTarget.x, WIDTH_BIAS);
   FJumping := false;
   assert(not assigned(FMoveTime));
   FMoveTime := TRpgTimestamp.Create(commons.round(MOVE_DELAY[FMoveRate] / 2.5));}
end;

function TAdditionSprite.canJump(which: TMoveOrder): boolean;

   procedure processJumpMove(const opcode: byte; var location: TPoint);
   var
      dummy: byte;
   begin
      if opcode in [0..7] then
         FLastJumpOp := opcode
      else if opcode in [8..$A] then
         FLastJumpOp := -1;
      case opcode of
         0: dec(location.Y);
         1: inc(location.X);
         2: inc(location.y);
         3: dec(location.x);
         4: location := location + TRpgPoint(point(1, -1));
         5: location := location + TRpgPoint(point(1, 1));
         6: location := location + TRpgPoint(point(1, -1));
         7: location := location + TRpgPoint(point(-1, -1));
         8:
         begin
            dummy := random(4);
            processJumpMove(dummy, location);
            FMoveDir := TFacing(dummy);
         end;
         9:
         begin
            dummy := ord(towardsHero);
            processJumpMove(dummy, location);
            FMoveDir := TFacing(dummy);
         end;
         $A:
         begin
            dummy := ord(opposite_facing(towardsHero));
            processJumpMove(dummy, location);
            FMoveDir := TFacing(dummy);
         end;
         $B:
         begin
            if FLastJumpOp = -1 then
               processJumpMove(ord(FMoveDir), location)
            else processJumpMove(FLastJumpOp, location);
         end;
      end; //end of CASE
   end;

var
   i: word;
   target: TPoint;
begin
   i := which.index;
   target := FLocation;
   FLastJumpOp := -1;
   while (i <= which.last) and (which.command[i].opcode <> $19) do
   begin
      if which.command[i].opcode in [0..$B] then
         processJumpMove(which.command[i].opcode, target);
      inc(i);
   end;
{   if (i <= which.last) and (pointInRect(target, rect(0, 0, GGameEngine.width, GGameEngine.height)))
      and TLowerTile(GGameEngine.tile[lower, target.x, target.y]).canEnter then
   begin
      result := true;
      FJumpTarget := target;
   end
   else result := false;}
   which.index := i;
end;

function TAdditionSprite.canMoveDiagonal(one, two: TFacing; out destination: TSgPoint): boolean;
var temp: TFacing;
begin
   assert((ord(one) mod 2 = 0) and (ord(two) mod 2 = 1));
   temp := FFacing;
   FFacing := one;
{   result := TGameMap(FEngine).canExit(FLocation.x, FLocation.y, one, self);
   if not result then
   begin
      FFacing := two;
      result := TGameMap(FEngine).canExit(FLocation.x, FLocation.y, two, self);
   end;
   FFacing := temp;
   if not result then
      Exit;

   destination := FLocation;
   if one = facing_up then
      dec(destination.Y)
   else inc(destination.Y);
   if two = facing_left then
      dec(destination.x)
   else inc(destination.x);
   result := ((pointInRect(destination, rect(0, 0, GGameEngine.width, GGameEngine.height))))
             and (TLowerTile(TGameMap(FEngine)[lower, destination.x, destination.y]).open(self));}
end;

function TAdditionSprite.canMoveForward: boolean;
begin
//   result := TGameMap(FEngine).canExit(FLocation.x, FLocation.y, FMoveDir, self);
end;

function TAdditionSprite.move(whichDir: TFacing): boolean;
var target: TPoint;
begin
   result := false;
   if assigned(FMoveTime) then
      Exit;
   if assigned(FPause) then
      Exit;

   if not self.dirLocked then
      facing := whichDir
   else FMoveDir := whichDir;
   if FSlipThrough or (self.canMoveForward) then
   begin
      self.leaveTile;
      target := point(FLocation.X, FLocation.Y);
      case FMoveDir of
         facing_up:
            dec(target.Y);
         facing_right:
            inc(target.X);
         facing_down:
            inc(target.Y);
         facing_left:
            dec(target.X);
      end;
      startMoveTo(target);
      result := true;
   end;
end;

function TAdditionSprite.moveDiag(one, two: TFacing): boolean;
var
   destination: TSgPoint;
begin
   result := false;
   if assigned(FMoveTime) then
      Exit;
   if assigned(FPause) then
      Exit;

   if (not self.dirLocked)and (not (facing in [one, two])) then
      facing := two;
   if FSlipThrough or (self.canMoveDiagonal(one, two, destination)) then
   begin
      self.leaveTile;
      self.startMoveTo(destination);
      result := true;
   end;
end;

procedure TAdditionSprite.startMoveTo(target: TSgPoint);
begin
   FLocation := target;
{   FTarget := target * TILESIZE;
   dec(FTarget.x, WIDTH_BIAS);
   assert(not assigned(FMoveTime));
   FMoveTime := TRpgTimestamp.Create(MOVE_DELAY[FMoveRate]);
   TLowerTile(TGameMap(FEngine)[lower, FLocation.x, FLocation.y]).event.Add(self);}
end;

procedure TAdditionSprite.stop;
begin
   FPaused := false;
   if assigned(FMoveAssignment) then
   begin
      if assigned(FMoveTime) then
         freeAndNil(FMoveTime)
      else if assigned(FPause) then
         freeAndNil(FPause);
      freeAndNil(FMoveAssignment);
   end;
end;

procedure TAdditionSprite.pause;
begin
   FPaused := true;
   if not assigned(FMoveAssignment) then
      if assigned(FPause) then
         FPause.pause
      else if assigned(FMoveTime) then
         FMoveTime.pause;
      //end if
   //end if
end;

procedure TAdditionSprite.resume;
begin
   FPaused := false;
   if not assigned(FMoveAssignment) then
      if assigned(FPause) then
         FPause.resume
      else if assigned(FMoveTime) then
         FMoveTime.resume;
      //end if
   //end if
end;

procedure TAdditionSprite.place;
var
   timeRemaining: cardinal;
   dummy: single;
   frequency: integer;
begin
   if not self.onMap then
      Exit;

   if not (FJumpAnimateOverride or self.animFix) then
   begin
      inc(FWhichFrame);
      if FWhichFrame = FOOTSTEP_CONSTANT[FMoveRate] then
      begin
         FWhichFrame := 0;
         FMoving := ((FMoving) + 3) mod 4;
      end;
   end;
   if assigned(FMoveTime) then
   begin
      timeRemaining := FMoveTime.timeRemaining;
      dummy := FTiles[bottom].x;
      moveTowards(timeRemaining, dummy, FTarget.x);
      FTiles[bottom].X := dummy;
      dummy := FTiles[bottom].y;
      moveTowards(timeRemaining, dummy, FTarget.y);
      FTiles[bottom].y := dummy;
{      if timeRemaining <= GFrameLength then
      begin
         freeAndNil(FMoveTime);
         if FJumping then
            endJump
         else begin
            if FMoveFreq < 8 then
            begin
               frequency := 8 - FMoveFreq;
               frequency := commons.powerWrap(2, frequency);
               FPause := TRpgTimestamp.Create(frequency * ((BASE_MOVE_DELAY - 15) div 4));
            end;
            FJumpAnimateOverride := false;
         end;
         TLowerTile(GGameEngine[lower, self.location.x, self.location.y]).bump(self);
      end;
   end;

   if not FInitialized then
   begin
      TLowerTile(TGameMap(FEngine)[lower, FLocation.x, FLocation.y]).event.Add(self);
      FInitialized := true;}
   end;
end;

function TAdditionSprite.doMove(which: TMoveOrder): boolean;
var
   dummySound: TRmSound;
   unchanged: boolean;
   orphan: TRpgCharacter;
begin
   result := true;
   unchanged := false;
   FMoveOpen := true;
   if FOrder.opcode = OP_CLEAR then
      FOrder := which.nextCommand;
   case FOrder.opcode of
      0: unchanged := not tryMove(facing_up);
      1: unchanged := not tryMove(facing_right);
      2: unchanged := not tryMove(facing_down);
      3: unchanged := not tryMove(facing_left);
      4: unchanged := not tryMoveDiagonal(facing_up, facing_right);
      5: unchanged := not tryMoveDiagonal(facing_down, facing_right);
      6: unchanged := not tryMoveDiagonal(facing_down, facing_left);
      7: unchanged := not tryMoveDiagonal(facing_up, facing_left);
      8: unchanged := not tryMove(TFacing(system.random(4)));
{      9: unchanged := not ((self = GParty.base) or (tryMove(towardsHero)));
      $A: unchanged := not ((self = GParty.base) or (tryMove(opposite_facing(towardsHero))));}
      $B: unchanged := not tryMove(FFacing);
      $C: self.facing := facing_up;
      $D: self.facing := facing_right;
      $E: self.facing := facing_down;
      $F: self.facing := facing_left;
      $10: self.facing := TFacing((ord(self.facing) + 1) mod 4);
      $11: self.facing := TFacing((ord(self.facing) - 1) mod 4);
      $12: self.facing := opposite_facing(self.facing);
      $13: self.facing := TFacing((ord(self.facing) + system.random(3) + 1) mod 4);
      $14: self.facing := TFacing(system.random(4));
      $15: self.facing := towardsHero;
      $16: self.facing := opposite_facing(towardsHero);
      $17:
      begin
         unchanged := true;
         if not assigned(FPause) then
            FPause := TRpgTimestamp.Create(100)
         else if FPause.timeRemaining = 0 then
         begin
            freeAndNil(FPause);
            unchanged := false;
         end;
      end;
      $18:
      begin
         if canJump(which) then
            beginJump;
      end;
      $19: {errLog('Hit an incorrect end of jump!');}; //They seem to move at 1.5X a sprite's base speed
      $1A: self.dirLocked := true;
      $1B: self.dirLocked := false;
      $1C: FMoveRate := min(6, FMoveRate + 1);
      $1D: FMoveRate := max(0, FMoveRate - 1);
      $1E: FMoveFreq := min(8, FMoveFreq + 1);
      $1F: FMoveFreq := min(0, FMoveFreq - 1);
{      $20:
      begin
         if between(FOrder.data[1], 0, high(GSwitches)) = FOrder.data[1] then
            GSwitches[FOrder.data[1]] := true;
      end;
      $21:
      begin
         if between(FOrder.data[1], 0, high(GSwitches)) = FOrder.data[1] then
            GSwitches[FOrder.data[1]] := false;
      end;
      $22:
      begin
         orphan := FCharacter;
         FCharacter.changeSprite(FOrder.name, FOrder.data[1]);
         self := orphan.base;
      end;
      $23:
      begin
         dummySound := TRmSound.Create(FOrder.name, 0, FOrder.data[1], FOrder.data[2], FOrder.data[3]);
         GCurrentEngine.mediaPlayer.playSfx(dummySound);
         dummySound.free;
      end;}
      $24: FSlipThrough := true;
      $25: FSlipThrough := false;
      $26: self.animFix := true;
      $27: self.animFix := false;
      $28: incTransparencyFactor;
      $29: decTransparencyFactor;
      $30: result := false;
      else assert(false);
   end;
   if not unchanged then
      FOrder.opcode := OP_CLEAR;
{   if (not FMoveOpen) and (FOrder.opcode <> $17) and (self.inFrontTile <> nil) then
      TLowerTile(self.inFrontTile).bump(self);}
   //end if
end;

procedure TAdditionSprite.moveTick;
begin
   if assigned(FMoveTime) then
      Exit;
   if assigned(FPause) then
   begin
      if FPause.timeRemaining = 0 then
         freeAndNil(FPause)
      else begin
         Exit;
      end;
   end;

   if assigned(FMoveQueue) and not FPaused then
   begin
      if not doMove(FMoveQueue) then
         freeAndNil(FMoveQueue);
   end else if assigned(FMoveAssignment) then
   begin
      if not doMove(FMoveAssignment) then
         freeAndNil(FMoveAssignment);
      //end if
   end
   else if (not FPaused) and self.hasPage then
      case FEvent.lastCurrentPage.moveType of
         still: ;
         cycle_ud:
            if FMoveReversed then
               FMoveOpen := self.move(facing_up)
            else FMoveOpen := self.move(facing_down);
         cycle_lr:
            if FMoveReversed then
               FMoveOpen := self.move(facing_right)
            else FMoveOpen := self.move(facing_left);
         randomMove, chase_hero, flee_hero, by_route: ; //handled elsewhere
      end; //end case
      if self.hasPage and (FEvent.lastCurrentPage.moveType in [cycle_ud, cycle_lr]) and (not FMoveOpen) then
      begin
         FMoveReversed := not FMoveReversed;
{         if self.inFrontTile <> nil then
            TLowerTile(self.inFrontTile).bump(self);}
      end;
   //end if
end;

procedure TAdditionSprite.nuke(removeself: boolean = false);
var
   dummy: integer;
   eventList: TList;
begin
//   eventList := TLowerTile(TGameMap(FEngine)[lower, FLocation.x, FLocation.y]).event;
   dummy := eventList.IndexOf(self);
   assert(dummy <> -1);
   eventList.Delete(dummy);
   eventList.Pack;
{   if removeSelf then
      GGameEngine.currentMap.deleteEvent(self);}
end;

function TAdditionSprite.isDirLocked: boolean;
begin
   result := (self.hasPage and (FEvent.lastCurrentPage.animType in [at_fixedDir..at_statue]))
             or FDirLocked;
end;

function TAdditionSprite.onMap: boolean;
begin
   result := (FLocation.X >= 0) and (FLocation.Y >= 0);
end;

destructor TAdditionSprite.Destroy;
begin
   if assigned(FTiles[bottom]) then
      FTiles[bottom].Dead;
   if assigned(FTiles[top]) then
      FTiles[top].Dead;
   FPause.free;
   FMoveTime.Free;
   FMoveQueue.Free;
   inherited;
end;

procedure TAdditionSprite.flash(r, g, b, power: byte; time: cardinal);
begin
{   if assigned(FTiles[top]) then
      (FTiles[top] as TTile).flash(r, g, b, power, time);
   if assigned(FTiles[bottom]) then
      (FTiles[bottom] as TTile).flash(r, g, b, power, time);}
end;

function TAdditionSprite.getAnimFix: boolean;
begin
   result := FAnimFix or (self.hasPage and (FEvent.lastCurrentPage.animType = at_statue));
end;

function TAdditionSprite.getBaseTile: TSprite;
begin
   result := FTiles[bottom];
end;

function TAdditionSprite.getCanSkip: boolean;
begin
   result := FCanSkip;
end;

function TAdditionSprite.getHasPage: boolean;
begin
   result := assigned(FEvent) and assigned(FEvent.lastCurrentPage);
end;

procedure TAdditionSprite.setFacing(data: TFacing);
begin
   if FUnderConstruction or not (self.hasPage and (FEvent.lastCurrentPage.animType in [at_fixedDir..at_statue])) then
      FFacing := data;
   FMoveDir := data;
end;

procedure TAdditionSprite.setLocation(data: TSgPoint);
begin
   FLocation := data;
//   FTarget := data * TILESIZE;
end;

procedure TAdditionSprite.setMovement(direction: TFacing);
begin
{   assert(self is THeroSprite);
   if not assigned(FMoveQueue) then
      FMoveQueue := TMoveOrder.Create(direction)
   else FMoveQueue.setDirection(direction);}
end;

procedure TAdditionSprite.setMoveOrder(const Value: TMoveOrder);
begin
   if assigned(FMoveAssignment) then
      freeAndNil(FMoveAssignment);
   FMoveAssignment := TMoveOrder.Assign(value);
end;

procedure TAdditionSprite.setTranslucency(const value: byte);
begin
   FTransparencyFactor := between(value, 0, MAX_TRANSPARENCY);
   FTiles[bottom].alpha := (8 - FTransparencyFactor) * TRANSPARENCY_STEP;
end;

procedure TAdditionSprite.setVisible(const Value: boolean);
begin
   FVisible := Value;
   if FTiles[top] <> nil then
      FTiles[top].Visible := value;
   if FTiles[bottom] <> nil then
      FTiles[bottom].Visible := value;
end;

function TAdditionSprite.towardsHero: TFacing;
var dummy: TPoint;
begin
{   if assigned(GGameEngine.currentParty) then
      dummy := point(trunc(GGameEngine.currentParty.baseTile.x), trunc(GGameEngine.currentParty.baseTile.y))
   else dummy := point(0, 100);
   result := towards(point(trunc(baseTile.x), trunc(baseTile.y)), dummy);}
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