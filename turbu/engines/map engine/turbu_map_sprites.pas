unit turbu_map_sprites;

interface
uses
   tiles, charset_data, timing,
   turbu_pathing, turbu_sprites, turbu_map_objects, turbu_defs, turbu_containers,
   sg_defs, sdl_sprite;

type
   TMapSprite = class;
   TMapSpriteList = class(TRpgObjectList<TMapSprite>);

   IRpgCharacter = interface
      procedure ChangeSprite(name: string; index: integer; oldSprite: TMapSprite);
   end;

   TMapSprite = class(TObject)
   private
      FMoveQueue: TPath;
      FMoveAssignment: TPath;
      FOrder: TMoveStep;
      FDirLocked: boolean;
      FMoveReversed: boolean;
      FMoveOpen: boolean;
      FFacing: TFacing;
      FJumping: boolean;
      FLastJumpOp: shortint;
      FJumpTarget: TSgPoint;
      FTransparencyFactor: byte;
      FPaused: boolean;

      function tryMove(where: TFacing): boolean; inline;
      function tryMoveDiagonal(one, two: TFacing): boolean; inline;
      function towardsHero: TFacing; inline;
      function canJump(which: TPath): boolean;
      function isDirLocked: boolean;
      procedure startMoveTo(target: TSgPoint); overload;
      procedure setMoveOrder(const Value: TPath);
      procedure beginJump;
      procedure endJump;
      procedure incTransparencyFactor;
      procedure decTransparencyFactor;
      function getAnimFix: boolean;
      function GetTile(x: byte): TTile;

      property dirLocked: boolean read isDirLocked write FDirLocked;
   protected
      FCharacter: IRpgCharacter;
      FLocation: TSgPoint;
      FTarget: TSgPoint;
      FMoveDir: TFacing;
      FTiles: array[1..2] of TTile;
      FMoving: byte;
      FEngine: TSpriteEngine;
      FMoveRate: byte;
      FMoveFreq: byte;
      FSlipThrough: boolean;
      FJumpAnimateOverride: boolean;
      FAnimFix: boolean;
      FMapObj: TRpgMapObject;
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
      function doMove(which: TPath): boolean; virtual;
      function getCanSkip: boolean; virtual;
      procedure setTranslucency(const value: byte); virtual;

      property animFix: boolean read getAnimFix write FAnimFix;
      property moveQueue: TPath read FMoveQueue;
      property moveAssign: TPath read FMoveAssignment;
   public
      constructor Create(base: TRpgMapObject; parent: TSpriteEngine; character: IRpgCharacter); virtual;
      destructor Destroy; override;
      function move(whichDir: TFacing): boolean; virtual;
      function moveDiag(one, two: TFacing): boolean;
      procedure leaveTile;
      procedure place; virtual;
      procedure updatePage(data: TRpgEventPage); virtual; abstract;
      procedure nuke(removeself: boolean = false);
      function inFront: TSgPoint;
      function inFrontTile: TTile; inline;
      function hasPage: boolean; inline;
      procedure flash(r, g, b, power: byte; time: cardinal);
      procedure moveTick;
      procedure update(filename: string; index: byte; transparent: boolean); virtual; abstract;
      procedure pause;
      procedure resume;
      procedure stop;

      property moveRate: byte read FMoveRate write FMoveRate;
      property moveFreq: byte read FMoveFreq write FMoveFreq;
      property location: TSgPoint read FLocation write setLocation;
      property event: TRpgMapObject read FMapObj;
      property baseTile: TSprite read getBaseTile;
      property visible: boolean read FVisible write setVisible;
      property facing: TFacing read FFacing write setFacing;
      property moveOrder: TPath read FMoveAssignment write setMoveOrder;
      property canSkip: boolean read getCanSkip write FCanSkip;
      property character: IRpgCharacter read FCharacter write FCharacter;
      property translucency: byte read FTransparencyFactor write setTranslucency;
      property tiles[x: byte]: TTile read GetTile;
   end;

   TEventSprite = class(TMapSprite)
   protected
      procedure setLocation(data: TSgPoint); override;
   public
      constructor Create(base: TRpgMapObject; parent: TSpriteEngine; character: IRpgCharacter); override;
      procedure updatePage(data: TRpgEventPage); override;
      procedure update(filename: string; index: byte; transparent: boolean); override;
   end;

   TCharSprite = class(TMapSprite)
   private
      FAnimFrame: TAnimFrame;

      procedure setAnimFrame(data: TAnimFrame);
      function workOutAnimFrame: TAnimFrame;
   protected
      FMoved: boolean;
      procedure setFacing(data: TFacing); override;
      procedure updateTiles;
      procedure setLocation(data: TSgPoint); override;
      procedure setTranslucency(const value: byte); override;
      procedure activateEvents(where: TTile);
   public
      constructor Create(base: TRpgMapObject; parent: TSpriteEngine; character: IRpgCharacter); override;
      procedure reload(const imageName: string; const index: byte);
      procedure assign(data: TCharSprite); reintroduce;
      procedure place; override;
      procedure update(filename: string; index: byte; transparent: boolean); override;
      procedure updatePage(data: TRpgEventPage); override;
//      procedure action(const button: TButtonCode = btn_enter); virtual; abstract;

      property frame: smallint read FWhichFrame;
      property animFrame: TAnimFrame read FAnimFrame write setAnimFrame;
   end;

const
   BASE_MOVE_DELAY = 133;
   MOVE_DELAY: array[1..6] of integer = (BASE_MOVE_DELAY * 8, BASE_MOVE_DELAY * 4, BASE_MOVE_DELAY * 2, BASE_MOVE_DELAY, BASE_MOVE_DELAY div 2, BASE_MOVE_DELAY div 4);
   WIDTH_BIAS = 4;
   FOOTSTEP_CONSTANT: array[1..6] of integer = (11, 10, 8, 6, 5, 5); //yay for fudge factors!
   MAX_TRANSPARENCY = 7;
   TRANSPARENCY_STEP = 255 div (MAX_TRANSPARENCY + 1);

implementation
uses
   SysUtils, Math, types,
   commons,
   turbu_2k_map_tiles,
   turbu_sounds, turbu_constants, turbu_2k_sprite_engine, turbu_2k_map_locks;

const OP_CLEAR = $C0; //arbitrary value

{ TMapSprite }

constructor TMapSprite.Create(base: TRpgMapObject; parent: TSpriteEngine;
  character: IRpgCharacter);
begin
   FCharacter := character;
   FMapObj := base;
   FEngine := parent as T2kSpriteEngine;
   FMoveRate := 4;
   FWhichFrame := -1;
   FOrder.opcode := OP_CLEAR;
   if self.hasPage then
   begin
      FMoveRate := FMapObj.currentPage.moveSpeed;
      FMoveFreq := FMapObj.currentPage.moveFrequency;
      FCanSkip := true;
      case FMapObj.currentPage.moveType of
         mt_still, mt_cycleUD, mt_cycleLR: ;
         mt_randomMove: FMoveQueue := TPath.Create(MOVE_CODES[MOVECODE_RANDOM], true);
         mt_chaseHero: FMoveQueue := TPath.Create(MOVE_CODES[MOVECODE_CHASE], true);
         mt_fleeHero: FMoveQueue := TPath.Create(MOVE_CODES[MOVECODE_FLEE], true);
         mt_byRoute:
         begin
            assert(assigned(FMapObj.currentPage.path));
            FMoveQueue := TPath.Assign(FMapObj.currentPage.path);
            FCanSkip := FMapObj.currentPage.moveIgnore;
         end;
         else assert(false);
      end;
   end;
end;

procedure TMapSprite.decTransparencyFactor;
begin
   self.translucency := max(FTransparencyFactor - 1, 0);
end;

procedure TMapSprite.incTransparencyFactor;
begin
   self.translucency := min(FTransparencyFactor + 1, MAX_TRANSPARENCY);
end;

function TMapSprite.inFront: TSgPoint;
begin
   case FFacing of
      facing_up: result := SgPoint(FLocation.x, FLocation.y - 1);
      facing_right: result := SgPoint(FLocation.x + 1, FLocation.y);
      facing_down: result := SgPoint(FLocation.x, FLocation.y + 1);
      facing_left: result := SgPoint(FLocation.x - 1, FLocation.y);
   end;
end;

function TMapSprite.inFrontTile: TTile;
var dummy: TSgPoint;
begin
   dummy := self.inFront;
   result := T2kSpriteEngine(FEngine).GetTile(dummy.x, dummy.y, 0);
end;

procedure TMapSprite.leaveTile;
var
   list: TMapSpriteList;
begin
   GEventLock.enter;
   try
      list := T2kSpriteEngine(FEngine).GetTile(location.x, location.y, 0).event;
      while list.indexOf(self) <> -1 do
         list.Remove(self);
   finally
      GEventLock.leave;
   end;
end;

function TMapSprite.tryMove(where: TFacing): boolean;
begin
   FMoveOpen := self.move(where);
   result := FMoveOpen or canSkip;
end;

function TMapSprite.tryMoveDiagonal(one, two: TFacing): boolean;
begin
   result := self.moveDiag(one, two) or canSkip;
end;

procedure TMapSprite.beginJump;
var midpoint: TSgPoint;
begin
   self.leaveTile;
   FJumping := true;
   FJumpAnimateOverride := true;
   midpoint := ((FJumpTarget - FLocation) * TILE_SIZE) / 2;
   dec(midpoint.Y, TILE_SIZE.Y div 2);
   FTarget := (FLocation * TILE_SIZE) + midpoint;
   dec(FTarget.x, WIDTH_BIAS);
   if not self.dirLocked and (FLocation <> FJumpTarget) then
      self.facing := towards(FLocation, FJumpTarget);
   FLocation := FJumpTarget;
   assert(not assigned(FMoveTime));
   TMapTile(T2kSpriteEngine(FEngine).GetTile(location.x, location.y, 0)).event.Add(self);
   FMoveTime := TRpgTimestamp.Create(commons.round(MOVE_DELAY[FMoveRate] / 2.5));
end;

procedure TMapSprite.endJump;
begin
   FTarget := FLocation * TILE_SIZE;
   dec(FTarget.x, WIDTH_BIAS);
   FJumping := false;
   assert(not assigned(FMoveTime));
   FMoveTime := TRpgTimestamp.Create(commons.round(MOVE_DELAY[FMoveRate] / 2.5));
end;

function TMapSprite.canJump(which: TPath): boolean;

   procedure processJumpMove(const opcode: byte; var location: TSgPoint);
   const
      UP_LEFT: TSgPoint = (x: 1; y: -1);
      UP_RIGHT: TSgPoint = (x: 1; y: 1);
      DOWN_LEFT: TSgPoint = (x: -1; y: -1);
      DOWN_RIGHT: TSgPoint = (x: -1; y: 1);
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
         4: location := location + UP_LEFT;
         5: location := location + UP_RIGHT;
         6: location := location + DOWN_LEFT;
         7: location := location + DOWN_RIGHT;
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
   target: TSgPoint;
begin
   i := which.cursor;
   target := FLocation;
   FLastJumpOp := -1;
   while (i <= which.last) and (which.opcodes[i].opcode <> $19) do
   begin
      if which.opcodes[i].opcode in [0..$B] then
         processJumpMove(which.opcodes[i].opcode, target);
      inc(i);
   end;
   if (i <= which.last) and (pointInRect(target, rect(0, 0, FEngine.width, FEngine.height)))
      and TMapTile(T2kSpriteEngine(FEngine).GetTile(location.x, location.y, 0)).canEnter then
   begin
      result := true;
      FJumpTarget := target;
   end
   else result := false;
   which.cursor := i;
end;

function TMapSprite.canMoveDiagonal(one, two: TFacing; out destination: TSgPoint): boolean;
var temp: TFacing;
begin
   assert((ord(one) mod 2 = 0) and (ord(two) mod 2 = 1));
   temp := FFacing;
   FFacing := one;
{$MESSAGE WARN 'Commented-out code in live unit'}
//   result := T2kSpriteEngine(FEngine).canExit(FLocation.x, FLocation.y, one, self);
   if not result then
   begin
      FFacing := two;
//      result := T2kSpriteEngine(FEngine).canExit(FLocation.x, FLocation.y, two, self);
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
   result := ((pointInRect(destination, rect(0, 0, FEngine.width, FEngine.height))))
             and (TMapTile(T2kSpriteEngine(FEngine).GetTile(destination.x, destination.y, 0)).open(self));
end;

function TMapSprite.canMoveForward: boolean;
begin
{$MESSAGE WARN 'Commented-out code in live unit'}
//   result := T2kSpriteEngine(FEngine).canExit(FLocation.x, FLocation.y, FMoveDir, self);
end;

function TMapSprite.move(whichDir: TFacing): boolean;
var target: TSgPoint;
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
      target := SgPoint(FLocation.X, FLocation.Y);
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

function TMapSprite.moveDiag(one, two: TFacing): boolean;
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

procedure TMapSprite.startMoveTo(target: TSgPoint);
begin
   FLocation := target;
   FTarget := target * TILE_SIZE;
   dec(FTarget.x, WIDTH_BIAS);
   assert(not assigned(FMoveTime));
   FMoveTime := TRpgTimestamp.Create(MOVE_DELAY[FMoveRate]);
   TMapTile(T2kSpriteEngine(FEngine).GetTile(FLocation.x, FLocation.y, 0)).event.Add(self);
end;

procedure TMapSprite.stop;
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

procedure TMapSprite.pause;
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

procedure TMapSprite.resume;
begin
   FPaused := false;
   if not assigned(FMoveAssignment) then
      if assigned(FPause) then
         FPause.resume
      else if assigned(FMoveTime) then
         FMoveTime.resume;
end;

procedure TMapSprite.place;
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
      dummy := FTiles[1].x;
      moveTowards(timeRemaining, dummy, FTarget.x);
      FTiles[1].X := dummy;
      dummy := FTiles[1].y;
      moveTowards(timeRemaining, dummy, FTarget.y);
      FTiles[1].y := dummy;
{$MESSAGE WARN 'Commented-out code in live unit'}
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
         TMapTile(GGameEngine[lower, self.location.x, self.location.y]).bump(self);
      end;
   end;

   if not FInitialized then
   begin
      TMapTile(TGameMap(FEngine)[lower, FLocation.x, FLocation.y]).event.Add(self);
      FInitialized := true;}
   end;
end;

function TMapSprite.doMove(which: TPath): boolean;
var
   dummySound: TRpgSound;
   unchanged: boolean;
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
{$MESSAGE WARN 'Commented-out code in live unit'}
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
{$MESSAGE WARN 'Commented-out code in live unit'}
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
      $22: FCharacter.changeSprite(FOrder.name, FOrder.data[1]);
      $23: GCurrentEngine.mediaPlayer.playAndFreeSfx(TRmSound.Create(FOrder.name, 0, FOrder.data[1],
                                                                     FOrder.data[2], FOrder.data[3]));
      }
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
   if (not FMoveOpen) and (FOrder.opcode <> $17) and (self.inFrontTile <> nil) then
      TMapTile(self.inFrontTile).bump(self);
end;

procedure TMapSprite.moveTick;
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
      case FMapObj.currentPage.moveType of
         mt_still: ;
         mt_cycleUD:
            if FMoveReversed then
               FMoveOpen := self.move(facing_up)
            else FMoveOpen := self.move(facing_down);
         mt_cycleLR:
            if FMoveReversed then
               FMoveOpen := self.move(facing_right)
            else FMoveOpen := self.move(facing_left);
         mt_randomMove, mt_chaseHero, mt_fleeHero, mt_byRoute: ; //handled elsewhere
      end; //end case
      if self.hasPage and (FMapObj.currentPage.moveType in [mt_cycleUD, mt_cycleLR]) and (not FMoveOpen) then
      begin
         FMoveReversed := not FMoveReversed;
         if self.inFrontTile <> nil then
            TMapTile(self.inFrontTile).bump(self);
      end;
   //end if
end;

procedure TMapSprite.nuke(removeself: boolean = false);
var
   dummy: integer;
   eventList: TMapSpriteList;
begin
   eventList := TMapTile(T2kSpriteEngine(FEngine).GetTile(FLocation.x, FLocation.y, 0)).event;
   dummy := eventList.IndexOf(self);
   assert(dummy <> -1);
   eventList.Delete(dummy);
{$MESSAGE WARN 'Commented-out code in live unit'}
{   if removeSelf then
      GGameEngine.currentMap.deleteEvent(self);}
end;

function TMapSprite.isDirLocked: boolean;
begin
   result := (self.hasPage and (FMapObj.currentPage.animType in [at_fixedDir..at_statue]))
             or FDirLocked;
end;

function TMapSprite.onMap: boolean;
begin
   result := (FLocation.X >= 0) and (FLocation.Y >= 0);
end;

destructor TMapSprite.Destroy;
begin
   if assigned(FTiles[1]) then
      FTiles[1].Dead;
   if assigned(FTiles[2]) then
      FTiles[2].Dead;
   FPause.free;
   FMoveTime.Free;
   FMoveQueue.Free;
   inherited;
end;

procedure TMapSprite.flash(r, g, b, power: byte; time: cardinal);
begin
   if assigned(FTiles[1]) then
      FTiles[1].flash(r, g, b, power, time);
   if assigned(FTiles[2]) then
      FTiles[2].flash(r, g, b, power, time);
end;

function TMapSprite.getAnimFix: boolean;
begin
   result := FAnimFix or (self.hasPage and (FMapObj.currentPage.animType = at_statue));
end;

function TMapSprite.getBaseTile: TSprite;
begin
   result := FTiles[1];
end;

function TMapSprite.getCanSkip: boolean;
begin
   result := FCanSkip;
end;

function TMapSprite.GetTile(x: byte): TTile;
begin
   result := FTiles[x];
end;

function TMapSprite.hasPage: boolean;
begin
   result := assigned(FMapObj) and assigned(FMapObj.currentPage);
end;

procedure TMapSprite.setFacing(data: TFacing);
begin
   if FUnderConstruction or not (self.hasPage and (FMapObj.currentPage.animType in [at_fixedDir..at_statue])) then
      FFacing := data;
   FMoveDir := data;
end;

procedure TMapSprite.setLocation(data: TSgPoint);
begin
   FLocation := data;
   FTarget := data * TILE_SIZE;
end;

procedure TMapSprite.setMoveOrder(const Value: TPath);
begin
   if assigned(FMoveAssignment) then
      freeAndNil(FMoveAssignment);
   FMoveAssignment := TPath.Assign(value);
end;

procedure TMapSprite.setTranslucency(const value: byte);
begin
   FTransparencyFactor := clamp(value, 0, MAX_TRANSPARENCY);
   FTiles[1].alpha := (8 - FTransparencyFactor) * TRANSPARENCY_STEP;
end;

procedure TMapSprite.setVisible(const Value: boolean);
begin
   FVisible := Value;
   if FTiles[2] <> nil then
      FTiles[2].Visible := value;
   if FTiles[1] <> nil then
      FTiles[1].Visible := value;
end;

function TMapSprite.towardsHero: TFacing;
//var dummy: TSgPoint;
begin
{$MESSAGE WARN 'Commented-out code in live unit'}
{   if assigned(GGameEngine.currentParty) then
      dummy := SgPoint(trunc(GGameEngine.currentParty.baseTile.x), trunc(GGameEngine.currentParty.baseTile.y))
   else dummy := SgPoint(0, 100);
   result := towards(SgPoint(trunc(baseTile.x), trunc(baseTile.y)), dummy);}
end;

{ TEventSprite }

constructor TEventSprite.create(base: TRpgMapObject; parent: TSpriteEngine; character: IRpgCharacter);
begin
   inherited Create(base, parent, character);
   FTiles[1] := TEventTile.create(Event, parent);
   FTiles[2] := nil;
   if assigned(FMapObj.currentPage) and FMapObj.currentPage.transparent then
      translucency := 3
   else translucency := 0;
   setLocation(point(base.location.X, base.location.Y));
end;

procedure TEventSprite.setLocation(data: TSgPoint);
begin
   inherited setLocation(data);
   FTiles[1].X := location.X * TILE_SIZE.x;
   FTiles[1].Y := location.Y * TILE_SIZE.y;
end;

procedure TEventSprite.update(filename: string; index: byte; transparent: boolean);
//var orphan: TRpgEvent;
begin
{$MESSAGE WARN 'Commented out code in live unit'}
{   self.translucency := 3 * ord(transparent);
   FMapObj.currentPage.overrideSprite(filename, index, transparent);
   orphan := FCharacter as TRpgEvent;
   orphan.switchType;
   orphan.changeSprite(filename, index);}
end;

procedure TEventSprite.updatePage(data: TRpgEventPage);
begin
   TEventTile(FTiles[1]).update(data);
end;

{ TCharSprite }

procedure TCharSprite.activateEvents(where: TTile);
var
   eventList: TMapSpriteList;
   i: integer;
   eventPtr: TRpgMapObject;
begin
{$MESSAGE WARN 'Commented out code in live unit'}
   eventList := (where as TMapTile).event;
   for i := 0 to eventlist.Count - 1 do
   begin
{      eventPtr := (eventList[i] as TMapSprite).event;
      if (eventList[i] <> self) and assigned(eventPtr.currentPage) and (eventPtr.currentPage.hasScript)
         and (eventPtr.currentPage.startCondition = by_key) then
         GScriptEngine.executeEvent(eventPtr, eventList[i] as TMapSprite);
      //end if }
   end;
end;

procedure TCharSprite.assign(data: TCharSprite);
begin
   begin
      self.facing := data.facing;
      FAnimFrame := data.animFrame;
      FLocation := data.location;
   end;
end;

constructor TCharSprite.create(base: TRpgMapObject; parent: TSpriteEngine; character: IRpgCharacter);
begin
   inherited Create(base, parent, character);
   FUnderConstruction := true;
   FTiles[1] := TEventTile.Create(base, parent);
   FTiles[2] := TEventTile.Create(base, parent);
   if base <> nil then
   begin
      assert(base.currentPage <> nil);
      if base.currentPage.transparent then
         translucency := 3;
      self.facing := base.currentPage.direction;
      updatePage(base.currentPage);
      setLocation(point(base.location.X, base.location.Y));
   end;
   FTiles[2].Z := 5;
   FTiles[1].Z := 4;
   FUnderConstruction := false;
end;

procedure TCharSprite.place;
begin
   FMoved := (FMoving > 0) and (FWhichFrame = FMoveRate - 1);
   inherited place;
   self.animFrame := self.workOutAnimFrame;
   FTiles[2].Y := FTiles[1].Y - TILE_SIZE.y;
   FTiles[2].X := FTiles[1].X;
end;

procedure TCharSprite.updateTiles;
begin
{$MESSAGE WARN 'Commented out code in live unit'}
   FTiles[2].ImageIndex := ord(self.facing) * 6 + ord(FAnimFrame);
   FTiles[1].ImageIndex := TEventTile(FTiles[2]).ImageIndex + 3;
end;

function TCharSprite.workOutAnimFrame: TAnimFrame;
var dummy: TAnimFrame;
begin
   result := center;
   dummy := center;
   case FMoving of
      3: dummy := left;
      2, 0: dummy := center;
      1: dummy := right;
   end;
   if not (FJumpAnimateOverride or self.animFix) then
   begin
      if hasPage then
         case FMapObj.currentPage.animType of
            at_sentry, at_fixedDir:
               if assigned(FMoveTime) then
                  result := dummy
               else result := center;
               //end if
            at_jogger, at_fixedJog: result := dummy;
            at_spinRight: self.facing := TFacing(3 - FMoving);
            at_statue: ;
         end
      else if not assigned(FMoveTime) and not assigned(FPause) and not assigned(moveQueue) and not assigned(moveAssign) then
         result := center
      else result := dummy;
   end;
end;

procedure TCharSprite.reload(const imageName: string; const index: byte);
begin
   FTiles[1].name := imagename + intToStr(index);
   FTiles[2].name := imagename + intToStr(index);
   self.facing := facing_down;
   self.FAnimFrame := center;
end;

procedure TCharSprite.setAnimFrame(data: TAnimFrame);
begin
   FAnimFrame := data;
   updateTiles;
end;

procedure TCharSprite.setFacing(data: TFacing);
begin
   inherited;
   updateTiles;
end;

procedure TCharSprite.setLocation(data: TSgPoint);
begin
   inherited setLocation(data);
   FTiles[1].X := (location.X * TILE_SIZE.x) - 4;
   FTiles[1].Y := location.Y * TILE_SIZE.y;
   FTiles[2].X := (location.X * TILE_SIZE.x) - 4; //correction for char width
   FTiles[2].Y := (location.Y - 1) * TILE_SIZE.y; //correction for double-height
end;

procedure TCharSprite.setTranslucency(const value: byte);
begin
   inherited setTranslucency(value);
   FTiles[2].Alpha := FTiles[1].Alpha;
end;

procedure TCharSprite.update(filename: string; index: byte; transparent: boolean);
begin
{$MESSAGE WARN 'Commented out code in live unit'}
{   loadCharset(filename);
   FTiles[2].ImageName := 'Charset ' + filename;
   FTiles[1].ImageName := 'Charset ' + filename;
   TMapTile(FTiles[2]).whichChar := index;
   TMapTile(FTiles[1]).whichChar := index;
   if transparent then
      self.translucency := 3
   else self.translucency := 0;
   updateTiles;}
end;

procedure TCharSprite.updatePage(data: TRpgEventPage);
var index: byte;
begin
   index := data.whichTile;
   FUnderConstruction := true;
   self.facing := data.direction;
   FUnderConstruction := false;
   update(data.name, index, translucency >= 3);
   FTiles[2].ImageIndex := data.whichTile * 2;
//   FTiles[2].ImageIndex := ord(data.direction) * 6 + ord(data.whichTile);
   FTiles[1].ImageIndex := TEventTile(FTiles[2]).ImageIndex + 3;
end;

end.
