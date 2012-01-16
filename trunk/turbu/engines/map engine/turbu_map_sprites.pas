unit turbu_map_sprites;

interface
uses
   OpenGL,
   rsImport,
   commons, tiles, charset_data, timing,
   turbu_pathing, turbu_sprites, turbu_map_objects, turbu_defs, turbu_containers,
   sg_defs, sdl_sprite;

type
   TMapSprite = class;

   TChangeSprite = procedure (name: string; translucent: boolean) of object;

   TMapSprite = class(TObject)
   private type
      TMoveChange = class
      private
         FPath: TPath;
         Ffrequency: integer;
         FLoop, FSkip: boolean;
      public
         constructor Create(path: TPath; frequency: integer; loop, skip: boolean);
      end;
   private
      FMoveQueue: TPath;
      FMoveAssignment: TPath;
      FMoveChange: TMoveChange;
      FOrder: TMoveStep;
      FDirLocked: boolean;
      FMoveReversed: boolean;
      FMoveOpen: boolean;
      FFacing: TFacing;
      FJumping: boolean;
      FJumpAnimateOverride: boolean;
      FLastJumpOp: shortint;
      FJumpTarget: TSgPoint;
      FJumpTime: integer;
      FTransparencyFactor: byte;
      FPaused: boolean;
      FInitialized: boolean;
      FMoveLoop: boolean;

      FFlashColor: TRpgColor;
      FFlashTimer: TRpgTimestamp;
      FFlashLength: integer;

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

      function MustFlash: boolean;
      function GetFlashColor: TGlArrayF4;
      procedure SetMovePause;
      procedure SetMoveQueue(const Value: TPath);

      property dirLocked: boolean read isDirLocked write FDirLocked;
   protected
      FChangeSprite: TChangeSprite;
      FLocation: TSgPoint;
      FTarget: TSgPoint;
      FMoveDir: TFacing;
      FTiles: array[1..2] of TEventTile;
      FEngine: TSpriteEngine;
      FMoveRate: byte;
      FMoveFreq: byte;
      FSlipThrough: boolean;
      FAnimFix: boolean;
      FMapObj: TRpgMapObject;
      FVisible: boolean;
      FPause: TRpgTimestamp;
      FMoveTime: TRpgTimestamp;
      FCanSkip: boolean;
      FUnderConstruction: boolean;

      procedure EnterTile;
      procedure setFacing(data: TFacing); virtual;
      procedure setVisible(const Value: boolean);
      function getBaseTile: TSprite; inline;
      procedure setLocation(data: TSgPoint); virtual;
      function canMoveForward: boolean; virtual;
      function canMoveDiagonal(one, two: TFacing; out destination: TSgPoint): boolean;
      function doMove(which: TPath): boolean; virtual;
      function getCanSkip: boolean; virtual;
      procedure setTranslucency(const value: byte); virtual;
      procedure SetFlashEvents(tile: TEventTile);
      procedure updateMove(data: TRpgEventPage);
      procedure DoUpdatePage(data: TRpgEventPage); virtual; abstract;

      property animFix: boolean read getAnimFix write FAnimFix;
      property moveQueue: TPath read FMoveQueue write SetMoveQueue;
      property moveAssign: TPath read FMoveAssignment;
   public
      [NoImport]
      constructor Create(base: TRpgMapObject; parent: TSpriteEngine); virtual;
      destructor Destroy; override;
      function move(whichDir: TFacing): boolean; virtual;
      function moveDiag(one, two: TFacing): boolean;
      procedure leaveTile;
      procedure place; virtual;
      procedure updatePage(data: TRpgEventPage);
      procedure nuke(removeself: boolean = false);
      function inFront: TSgPoint;
      function inFrontTile: TTile; inline;
      function hasPage: boolean; inline;
      procedure flash(r, g, b, power: byte; time: cardinal);
      procedure moveTick; virtual;
      procedure update(filename: string; transparent: boolean); virtual; abstract;
      procedure pause;
      procedure resume;
      procedure stop;
      procedure CopyDrawState(base: TMapSprite);
      procedure MoveChange(path: TPath; frequency: integer; loop, skip: boolean);
      procedure CheckMoveChange;

      property moveRate: byte read FMoveRate;
      property moveFreq: byte read FMoveFreq write FMoveFreq;
      property location: TSgPoint read FLocation write setLocation;
      property event: TRpgMapObject read FMapObj;
      property baseTile: TSprite read getBaseTile;
      property visible: boolean read FVisible write setVisible;
      property facing: TFacing read FFacing write setFacing;
      property moveOrder: TPath read FMoveAssignment write setMoveOrder;
      property canSkip: boolean read getCanSkip write FCanSkip;
      property moveLoop: boolean read FMoveLoop write FMoveLoop;
      property translucency: byte read FTransparencyFactor write setTranslucency;
      property tiles[x: byte]: TTile read GetTile;
      property OnChangeSprite: TChangeSprite read FChangeSprite write FChangeSprite;
   end;

   TEventSprite = class(TMapSprite)
   protected
      procedure setLocation(data: TSgPoint); override;
      procedure DoUpdatePage(data: TRpgEventPage); override;
   public
      constructor Create(base: TRpgMapObject; parent: TSpriteEngine); override;
      procedure update(filename: string; transparent: boolean); override;
   end;

   TCharSprite = class(TMapSprite)
   private
      FWhichFrame: smallint;
      FAnimTimer: TRpgTimestamp;

      procedure loadCharset(filename: string);
      procedure UpdateFrame;
   protected
      FMoved: boolean;
      FActionMatrix: TMoveMatrix;
      FAction: integer;
      FMoveFrame: byte;
      procedure setFacing(data: TFacing); override;
      procedure updateTiles;
      procedure setLocation(data: TSgPoint); override;
      procedure setTranslucency(const value: byte); override;
      procedure activateEvents(where: TTile);
      procedure DoUpdatePage(data: TRpgEventPage); override;
   public
      constructor Create(base: TRpgMapObject; parent: TSpriteEngine); override;
      destructor Destroy; override;
      procedure reload(const imageName: string; const index: byte);
      procedure assign(data: TCharSprite); reintroduce;
      procedure place; override;
      procedure update(filename: string; transparent: boolean); override;
      procedure action(const button: TButtonCode = btn_enter); virtual;
      procedure moveTick; override;

      property frame: smallint read FWhichFrame;
   end;

const
   BASE_MOVE_DELAY = 133;
   MOVE_DELAY: array[1..6] of integer = (333, 243, 177, 133, 111, 67);
   WIDTH_BIAS = 4;
   FOOTSTEP_CONSTANT: array[1..6] of integer = (11, 10, 8, 6, 5, 5); //yay for fudge factors!
   MAX_TRANSPARENCY = 7;
   TRANSPARENCY_STEP = 255 div (MAX_TRANSPARENCY + 1);

implementation
uses
   SysUtils, Math, types,
   turbu_2k_map_tiles, ArchiveInterface,
   turbu_sounds, turbu_constants, turbu_2k_sprite_engine, turbu_2k_map_locks,
   turbu_database, turbu_2k_environment,
   rs_media;

const OP_CLEAR = $C0; //arbitrary value

type
   TMapSpriteHelper = class helper for TMapSprite
   public
      function currentTile: TMapTile;
   end;

{ TMapSprite }

procedure TMapSprite.CopyDrawState(base: TMapSprite);
begin
   if assigned(base.FFlashTimer) then
   begin
      FFlashTimer := TRpgTimestamp.Create(base.FFlashTimer.timeRemaining);
      FFlashColor := base.FFlashColor;
      FFlashLength := base.FFlashLength;
   end;
   if assigned(base.FMoveQueue) then
      FMoveQueue := base.FMoveQueue.clone;
   if assigned(base.FMoveAssignment) then
   begin
      FMoveAssignment := base.FMoveAssignment.clone;
      FMoveFreq := base.FMoveFreq;
   end;
   FMoveReversed := base.FMoveReversed;
   FMoveOpen := base.FMoveOpen;
   FMoveLoop := base.FMoveLoop;
   FMoveDir := base.FMoveDir;
   if base is TCharSprite then
      self.facing := base.facing;
   self.location := base.location;
end;

constructor TMapSprite.Create(base: TRpgMapObject; parent: TSpriteEngine);
begin
   FMapObj := base;
   FEngine := parent as T2kSpriteEngine;
   FMoveRate := 4;
   FOrder.opcode := OP_CLEAR;
   if self.hasPage then
      UpdateMove(FMapObj.CurrentPage);
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
begin
   GEventLock.enter;
   try
      GSpriteEngine.LeaveLocation(currentTile.Location, self);
   finally
      GEventLock.leave;
   end;
end;

procedure TMapSprite.EnterTile;
begin
   GEventLock.enter;
   try
      GSpriteEngine.AddLocation(currentTile.Location, self);
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

procedure TMapSprite.updateMove(data: TRpgEventPage);
begin
   FMoveRate := data.moveSpeed;
   FMoveFreq := data.moveFrequency;
   FCanSkip := true;
   case data.moveType of
      mt_still, mt_cycleUD, mt_cycleLR: SetMoveQueue(nil);
      mt_randomMove: SetMoveQueue(TPath.Create(MOVE_CODES[MOVECODE_RANDOM], true));
      mt_chaseHero: SetMoveQueue(TPath.Create(MOVE_CODES[MOVECODE_CHASE], true));
      mt_fleeHero: SetMoveQueue(TPath.Create(MOVE_CODES[MOVECODE_FLEE], true));
      mt_byRoute:
      begin
         assert(assigned(data.path));
         SetMoveQueue(TPath.Assign(data.path));
         FCanSkip := data.moveIgnore;
      end;
      else assert(false);
   end;
end;

procedure TMapSprite.CheckMoveChange;
begin
   if assigned(FMoveChange) then
   begin
      self.moveOrder := FMoveChange.FPath;
      self.canSkip := FMoveChange.FSkip;
      self.moveLoop := FMoveChange.FLoop;
      self.moveFreq := FMoveChange.Ffrequency;
      FreeAndNil(FMoveChange);
   end;
end;

procedure TMapSprite.updatePage(data: TRpgEventPage);
begin
   DoUpdatePage(data);
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
   GSpriteEngine.Addlocation(currentTile.location, self);
   FMoveTime := TRpgTimestamp.Create(FJumpTime);
end;

procedure TMapSprite.endJump;
begin
   FTarget := FLocation * TILE_SIZE;
   dec(FTarget.x, WIDTH_BIAS);
   FJumping := false;
   assert(not assigned(FMoveTime));
   FMoveTime := TRpgTimestamp.Create(FJumpTime);
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
      end;
   end;

var
   i: word;
   target: TSgPoint;
begin
   i := which.cursor;
   target := FLocation;
   FLastJumpOp := -1;
   FJumpTime := commons.round(MOVE_DELAY[FMoveRate] / 1.5);
   while (i <= which.last) and (which.opcodes[i].opcode <> $19) do
   begin
      if which.opcodes[i].opcode in [0..$B] then
         processJumpMove(which.opcodes[i].opcode, target);
      inc(i);
   end;
   if (i <= which.last) and (pointInRect(target, rect(0, 0, FEngine.width, FEngine.height)))
      and currentTile.canEnter then
   begin
      result := true;
      FJumpTarget := target;
   end
   else result := false;
   if which.opcodes[i].opcode = $19 then
      inc(i);
   which.cursor := i;
end;

function TMapSprite.canMoveDiagonal(one, two: TFacing; out destination: TSgPoint): boolean;
var temp: TFacing;
begin
   assert((ord(one) mod 2 = 0) and (ord(two) mod 2 = 1));
   temp := FFacing;
   FFacing := one;
   result := T2kSpriteEngine(FEngine).canExit(FLocation.x, FLocation.y, one, self);
   if not result then
   begin
      FFacing := two;
      result := T2kSpriteEngine(FEngine).canExit(FLocation.x, FLocation.y, two, self);
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
             and currentTile.open(self);
end;

function TMapSprite.canMoveForward: boolean;
begin
   result := T2kSpriteEngine(FEngine).canExit(FLocation.x, FLocation.y, FMoveDir, self);
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
         facing_up: dec(target.Y);
         facing_right: inc(target.X);
         facing_down: inc(target.Y);
         facing_left: dec(target.X);
      end;
      startMoveTo(target);
      result := true;
   end;
end;

procedure TMapSprite.MoveChange(path: TPath; frequency: integer; loop,
  skip: boolean);
begin
   FreeAndNil(FMoveChange);
   FMoveChange := TMoveChange.Create(path, frequency, loop, skip);
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
var
   lTarget: TSgPoint;
begin
   FLocation := target;
   lTarget := target;
   GSpriteEngine.normalizePoint(lTarget.x, lTarget.y);
   FTarget := lTarget * TILE_SIZE;
   if lTarget <> target then
   begin
      case FMoveDir of
         facing_up: dec(lTarget.y);
         facing_right: dec(lTarget.x);
         facing_down: inc(lTarget.y);
         facing_left: inc(lTarget.x);
      end;
      SetLocation(lTarget);
   end;
   dec(FTarget.x, WIDTH_BIAS);
   assert(not assigned(FMoveTime));
   FMoveTime := TRpgTimestamp.Create(MOVE_DELAY[FMoveRate]);
   EnterTile;
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

procedure TMapSprite.SetMovePause;
var
   frequency: integer;
begin
   if (FMoveFreq < 8) then
   begin
      FPause.Free;
      frequency := 8 - FMoveFreq;
      frequency := commons.powerWrap(2, frequency);
      FPause := TRpgTimestamp.Create(frequency * ((BASE_MOVE_DELAY - 15) div 4));
   end;
end;

procedure TMapSprite.SetMoveQueue(const Value: TPath);
begin
   FMoveQueue.Free;
   FMoveQueue := Value;
end;

procedure TMapSprite.place;
var
   timeRemaining: integer;
   dummy: single;
begin
   if (FInitialized and not assigned(FMoveTime)) or
      (FInitialized and not (assigned(FMoveQueue) or assigned(FMoveAssignment))
      and assigned(FMapObj) and (FMapObj.currentPage.moveType = mt_still)) then
   begin
      Exit;
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
      if timeRemaining <= TRpgTimeStamp.FrameLength then
      begin
         freeAndNil(FMoveTime);
         if FJumping then
            endJump
         else begin
            SetMovePause;
            FJumpAnimateOverride := false;
         end;
         currentTile.bump(self);
      end;
   end;

   if not FInitialized then
   begin
      EnterTile;
      FInitialized := true;
   end;
end;

function TMapSprite.doMove(which: TPath): boolean;
var
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
      9: unchanged := not ((self = GSpriteEngine.CurrentParty) or (tryMove(towardsHero)));
      $A: unchanged := not ((self = GSpriteEngine.CurrentParty) or (tryMove(opposite_facing(towardsHero))));
      $B: unchanged := not tryMove(FFacing);
      $C: self.facing := facing_up;
      $D: self.facing := facing_right;
      $E: self.facing := facing_down;
      $F: self.facing := facing_left;
      $10: self.facing := TFacing((ord(self.facing) + 1) mod 4);
      $11: self.facing := TFacing((ord(self.facing) + ord(pred(high(TFacing)))) mod 4);
      $12: self.facing := opposite_facing(self.facing);
      $13: self.facing := TFacing((ord(self.facing) + system.random(3) + 1) mod 4);
      $14: self.facing := TFacing(system.random(4));
      $15: self.facing := towardsHero;
      $16: self.facing := opposite_facing(towardsHero);
      $17:
      begin
         assert(not assigned(FPause));
         FPause := TRpgTimestamp.Create(300)
      end;
      $18:
      begin
         if canJump(which) then
            beginJump;
      end;
      $19: assert(false, 'Hit an incorrect end of jump!'); //They seem to move at 1.5X a sprite's base speed
      $1A: self.dirLocked := true;
      $1B: self.dirLocked := false;
      $1C: FMoveRate := min(6, FMoveRate + 1);
      $1D: FMoveRate := max(0, FMoveRate - 1);
      $1E: FMoveFreq := min(8, FMoveFreq + 1);
      $1F: FMoveFreq := min(0, FMoveFreq - 1);
      $20: GEnvironment.Switch[FOrder.data[1]] := true;
      $21: GEnvironment.Switch[FOrder.data[1]] := false;
      $22: FChangeSprite(FOrder.name, boolean(FOrder.data[1]));
      $23: rs_Media.playSound(FOrder.name, FOrder.data[1], FOrder.data[2], FOrder.data[3]);
      $24: FSlipThrough := true;
      $25: FSlipThrough := false;
      $26: self.animFix := true;
      $27: self.animFix := false;
      $28: incTransparencyFactor;
      $29: decTransparencyFactor;
      $30: result := false;
      else assert(false);
   end;
   if FOrder.opcode in [$C..$16] then
   begin
      FPause.Free;
      FPause := TRpgTimestamp.Create(MOVE_DELAY[FMoveRate] div 3);
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
      if (FPause.timeRemaining > 0) then
         Exit
      else FreeAndNil(FPause);
   end;

   if assigned(FMoveQueue) and not FPaused then
   begin
      if not doMove(FMoveQueue) then
         freeAndNil(FMoveQueue);
   end else if assigned(FMoveAssignment) then
   begin
      if not doMove(FMoveAssignment) then
         freeAndNil(FMoveAssignment);
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
         else ; //handled elsewhere
      end;
   if self.hasPage and (FMapObj.currentPage.moveType in [mt_cycleUD, mt_cycleLR]) and (not FMoveOpen) then
   begin
      FMoveReversed := not FMoveReversed;
      if self.inFrontTile <> nil then
         TMapTile(self.inFrontTile).bump(self);
   end;
end;

function TMapSprite.MustFlash: boolean;
begin
   result := Assigned(FFlashTimer) and (FFlashTimer.timeRemaining > 0);
end;

procedure TMapSprite.nuke(removeself: boolean = false);
begin
   GSpriteEngine.deleteMapObject(self);
end;

function TMapSprite.isDirLocked: boolean;
begin
   result := (self.hasPage and (FMapObj.currentPage.animType in [at_fixedDir..at_statue]))
             or FDirLocked;
end;

destructor TMapSprite.Destroy;
begin
   if assigned(FTiles[1]) then
      FTiles[1].Dead;
   if assigned(FTiles[2]) then
      FTiles[2].Dead;
   FFlashTimer.Free;
   FPause.free;
   FMoveTime.Free;
   FMoveQueue.Free;
   inherited;
end;

procedure TMapSprite.flash(r, g, b, power: byte; time: cardinal);
begin
   if time = 0 then
   begin
      FreeAndNil(FFlashTimer);
      Exit;
   end;
   FFlashColor.rgba[1] := r;
   FFlashColor.rgba[2] := g;
   FFlashColor.rgba[3] := b;
   FFlashColor.rgba[4] := power;
   FFlashTimer.Free;
   time := time * 100;
   FFlashTimer := TRpgTimestamp.Create(time);
   FFlashLength := time;
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

function TMapSprite.GetFlashColor: TGlArrayF4;
begin
   result[0] := (FFlashColor.rgba[1] / 255);
   result[1] := (FFlashColor.rgba[2] / 255);
   result[2] := (FFlashColor.rgba[3] / 255);
   result[3] := (FFlashColor.rgba[4] / 255) * (FFlashTimer.timeRemaining / FFlashLength);
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

procedure TMapSprite.SetFlashEvents(tile: TEventTile);
begin
   tile.OnMustFlash := self.MustFlash;
   tile.OnFlashColor := self.GetFlashColor;
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
   FMoveAssignment := value;
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
var
   heroLoc: TSgPoint;
begin
   if assigned(GSpriteEngine.currentParty) then
      heroLoc := GSpriteEngine.currentParty.location
   else heroLoc := SgPoint(0, 1000);
   result := towards(self.location, heroLoc);
end;

{ TEventSprite }

constructor TEventSprite.create(base: TRpgMapObject; parent: TSpriteEngine);
begin
   inherited Create(base, parent);
   FTiles[1] := TEventTile.create(Event, parent);
   FTiles[2] := nil;
   if assigned(FMapObj.currentPage) and FMapObj.currentPage.transparent then
      translucency := 3
   else translucency := 0;
   setLocation(point(base.location.X, base.location.Y));
   self.SetFlashEvents(FTiles[1]);
end;

procedure TEventSprite.setLocation(data: TSgPoint);
begin
   inherited setLocation(data);
   FTiles[1].X := location.X * TILE_SIZE.x;
   FTiles[1].Y := location.Y * TILE_SIZE.y;
end;

procedure TEventSprite.update(filename: string; transparent: boolean);
begin
   self.translucency := 3 * ord(transparent);
   assert(false);
end;

procedure TEventSprite.DoUpdatePage(data: TRpgEventPage);
begin
   FTiles[1].update(data);
   updateMove(data);
end;

{ TCharSprite }

procedure TCharSprite.action(const button: TButtonCode);
begin
   raise ESpriteError.Create('Non-player sprites can''t receive an Action.');
end;

procedure TCharSprite.activateEvents(where: TTile);
var
   eventList: TArray<TMapSprite>;
   i: integer;
   eventPtr: TRpgMapObject;
begin
   eventList := (where as TMapTile).event;
   for i := 0 to high(eventlist) do
   begin
      eventPtr := (eventList[i]).event;
      if (eventList[i] <> self) and assigned(eventPtr.currentPage) and (eventPtr.currentPage.hasScript)
         and (eventPtr.currentPage.startCondition = by_key) then
{$MESSAGE WARN 'Commented out code in live unit'}
         {GScriptEngine.executeEvent(eventPtr, eventList[i])};
   end;
end;

procedure TCharSprite.assign(data: TCharSprite);
begin
   begin
      self.facing := data.facing;
      FLocation := data.location;
   end;
end;

constructor TCharSprite.create(base: TRpgMapObject; parent: TSpriteEngine);
begin
   inherited Create(base, parent);
   FUnderConstruction := true;
   FWhichFrame := -1;
   FTiles[1] := TEventTile.Create(base, parent);
   FTiles[2] := TEventTile.Create(base, parent);
   if assigned(base) and assigned(base.currentPage) then
   begin
      if base.currentPage.transparent then
         translucency := 3;
      FActionMatrix := GDatabase.moveMatrix[FMapObj.CurrentPage.ActionMatrix];
      self.facing := base.currentPage.direction;
      updatePage(base.currentPage);
      setLocation(point(base.location.X, base.location.Y));
   end
   else FActionMatrix := GDatabase.moveMatrix[0];
   FTiles[2].Z := 5;
   FTiles[1].Z := 4;
   FUnderConstruction := false;
   self.SetFlashEvents(FTiles[1]);
   self.SetFlashEvents(FTiles[2]);
   FAnimTimer := TRpgTimestamp.Create(0);
end;

destructor TCharSprite.Destroy;
begin
   FAnimTimer.Free;
   inherited;
end;

procedure TCharSprite.loadCharset(filename: string);
begin
   FEngine.Images.EnsureImage('mapsprite\' + filename + '.png', filename, SPRITE_SIZE);
end;

procedure TCharSprite.moveTick;
begin
   inherited moveTick;
   UpdateTiles;
end;

procedure TCharSprite.UpdateFrame;
const
   ANIM_DELAY: array[1..6] of integer = (208, 125, 104, 78, 57, 42);
   TIME_FACTOR = 7;
var
   newFrame, moveDelay: integer;
begin
   if (FJumpAnimateOverride or self.animFix) then
      Exit;
   if FAnimTimer.timeRemaining > 0 then
      Exit;

   inc(FWhichFrame);
   if FWhichFrame = FOOTSTEP_CONSTANT[FMoveRate] then
   begin
      FWhichFrame := 0;
      newFrame := (FMoveFrame + 1) mod length(FActionMatrix[FAction]);
   end
   else newFrame := FMoveFrame;

   moveDelay := ANIM_DELAY[FMoveRate];
   if hasPage then
      case FMapObj.currentPage.animType of
         at_sentry, at_fixedDir:
            if assigned(FMoveTime) then
               FMoveFrame := newFrame
            else FMoveFrame := 0;
         at_jogger, at_fixedJog: FMoveFrame := newFrame;
         at_spinRight:
         begin
            self.facing := TFacing((ord(self.facing) + 1) mod ord(high(TFacing)));
            moveDelay := moveDelay * 10
         end;
         at_statue: ;
      end
   else if not assigned(FMoveTime) and not assigned(moveQueue)
     and not (assigned(moveAssign) and (FTarget <> FLocation * TILE_SIZE)) then
      FMoveFrame := 0
   else FMoveFrame := newFrame;
   FAnimTimer.Create(moveDelay div TIME_FACTOR);
end;

procedure TCharSprite.place;
begin
   FMoved := (FMoveFrame > 0) and (FWhichFrame = FMoveRate - 1);
   inherited place;
   UpdateFrame;
   FTiles[2].Y := FTiles[1].Y - TILE_SIZE.y;
   FTiles[2].X := FTiles[1].X;
end;

procedure TCharSprite.updateTiles;
var
   frame: integer;
begin
   frame := FActionMatrix[FAction, FMoveFrame] * 2;
   FTiles[2].ImageIndex := frame;
   FTiles[1].ImageIndex := frame + 1;
end;

procedure TCharSprite.reload(const imageName: string; const index: byte);
begin
   FTiles[1].name := imagename + intToStr(index);
   FTiles[2].name := imagename + intToStr(index);
   self.facing := facing_down;
   FMoveFrame := 0;
end;

procedure TCharSprite.setFacing(data: TFacing);
begin
   inherited setFacing(data);
   FAction := ord(facing);
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

procedure TCharSprite.update(filename: string; transparent: boolean);
begin
   loadCharset(filename);
   FTiles[2].ImageName := filename;
   FTiles[1].ImageName := filename;
   if transparent then
      self.translucency := 3
   else self.translucency := 0;
   updateTiles;
end;

procedure TCharSprite.DoUpdatePage(data: TRpgEventPage);
begin
   if assigned(data) then
   begin
      FUnderConstruction := true;
      self.facing := data.direction;
      FMoveFrame := data.whichTile;
      UpdateMove(data);
      FUnderConstruction := false;
      update(data.name, translucency >= 3);
   end;
   self.visible := assigned(data);
end;

{ TMapSpriteHelper }

function TMapSpriteHelper.currentTile: TMapTile;
begin
   result := T2kSpriteEngine(FEngine)[0, self.location.x, self.location.y];
end;

{ TMapSprite.TMoveChange }

constructor TMapSprite.TMoveChange.Create(path: TPath; frequency: integer; loop,
  skip: boolean);
begin
   FPath := path;
   Ffrequency := frequency;
   FLoop := loop;
   FSkip := skip;
end;

end.
