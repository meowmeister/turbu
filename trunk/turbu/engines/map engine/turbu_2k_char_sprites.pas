unit turbu_2k_char_sprites;

interface
uses
   Types, SysUtils,
   commons, tiles, turbu_2k_map_tiles, turbu_map_sprites, turbu_mapchars,
   turbu_defs, turbu_heroes, turbu_pathing,
   sdl_sprite, SG_defs;

type
   TVehicleState = (vs_empty, vs_launching, vs_active, vs_landing, vs_emptying);

   TVehicleSprite = class;

   TVehicleTile = class(TEventTile)
   protected
      FOwner: TVehicleSprite;
      FOffset: TsgPoint;
      FOnCleanup: TProc;

      procedure offsetTowards(const offset: TsgPoint; const state: TVehicleState);
   public
      constructor Create(const base: TEventTile; parent: TSpriteEngine;
         const onCleanup: TProc); reintroduce;
      destructor Destroy; override;
      procedure Draw; override;
   end;

   THeroSprite = class;

   TUnloadMethod = (um_here, um_front);

   TVehicleSprite = class (TCharSprite)
   private
      FTemplate: TRpgVehicle;
      FCarrying: THeroSprite;
      FStateCounter: integer;
      FAltitude: integer;
      FShadow: TSprite;
      FOnCleanup: TProc;

      procedure setState(const Value: TVehicleState);
      function unloadLocation: TSgPoint;
   protected
      FState: TVehicleState;
      FAnimated: boolean;
      FAnimDir: boolean;
      FAnimCounter: byte;
      FUnloadMethod: TUnloadMethod;

      function canMoveForward: boolean; override;
      procedure reportState(which: TVehicleState);
      function unload: boolean;
      procedure doAction(const button: TButtonCode);
      procedure bump(bumper: TMapSprite); override;
      procedure SetTarget(const value: TsgPoint); override;
   public
      constructor Create(parent: TSpriteEngine; whichVehicle: TRpgVehicle;
         const cleanup: TProc); reintroduce;
      destructor Destroy; override;
      procedure launch;
      procedure place; override;
      procedure action(const button: TButtonCode = btn_enter); override; final;
      procedure setLocation(data: TSgPoint); override;

      property state: TVehicleState read FState write setState;
      property template: TRpgVehicle read FTemplate;
   end;

   THeroSprite = class(TCharSprite)
   private
      FTemplate: TRpgHero;
      FParty: TRpgParty;
      FNextMove: TFacing;
      FMoveQueued: boolean;
      FMoveTick: boolean;

      procedure rideVehicle(theVehicle: TVehicleSprite);
      procedure queueMove(direction: TFacing); inline;
      procedure setMovement(direction: TFacing);
   protected
      function doMove(which: TPath): boolean; override;
      function getCanSkip: boolean; override;
   public
      constructor create(const AParent: TSpriteEngine; whichHero: TRpgHero; party: TRpgParty); reintroduce;
      destructor Destroy; override;
      procedure action(const button: TButtonCode = btn_enter); override; final;
      procedure place; override;
      procedure boardVehicle;
      function move(whichDir: TFacing): boolean; override;
      procedure packUp;
      procedure settleDown(engine: TSpriteEngine);

      property template: TRpgHero read FTemplate;
   end;

implementation
uses
   turbu_2k_sprite_engine, timing, turbu_database, turbu_sprites,
   turbu_characters, turbu_terrain,
   turbu_2k_environment;

const
   AIRSHIP_OFFSET: TsgPoint = (x: 0; y: -16);
   VEHICLE_ANIM_RATE = 10;

{ TVehicleTile }

constructor TVehicleTile.Create(const base: TEventTile; parent: TSpriteEngine;
   const onCleanup: TProc);
begin
   inherited Create(base.event, parent as T2kSpriteEngine);
   self.z := 3;
   self.Assign(base);
   FOnCleanup := onCleanup;
end;

procedure TVehicleTile.offsetTowards(const offset: TsgPoint; const state: TVehicleState);
var displacement: shortint;
begin
   if FOffset.x <> offset.x then
   begin
      displacement := offset.x - FOffset.x;
      displacement := displacement div abs(displacement);
      inc(FOffset.x, displacement);
      self.x := self.x + displacement;
   end;
   if FOffset.y <> offset.y then
   begin
      displacement := offset.y - FOffset.Y;
      displacement := displacement div abs(displacement);
      inc(FOffset.y, displacement);
      self.y := self.y + displacement;
   end;
   if (FOffset.x = offset.x) and (FOffset.y = offset.y) then
      FOwner.reportState(state);
end;

{ TVehicleSprite }

constructor TVehicleSprite.Create(parent: TSpriteEngine; whichVehicle: TRpgVehicle;
   const cleanup: TProc);
var
   newTile: TVehicleTile;
begin
   inherited Create(nil, parent);
   self.OnChangeSprite := whichVehicle.ChangeSprite;
   whichVehicle.gamesprite := self;
   self.FTemplate := whichVehicle;
   //update char tiles to vehicle tiles
   newTile := TVehicleTile.Create(FTiles[2] as TEventTile, parent,
      procedure begin FTiles[2] := nil end);
   newTile.FOwner := self;
   FTiles[2].Free;
   FTiles[2] := newTile;
   newTile := TVehicleTile.Create(FTiles[1] as TEventTile, parent,
      procedure begin FTiles[1] := nil end);
   newTile.FOwner := self;
   FTiles[1].Free;
   FTiles[1] := newTile;

   visible := (whichVehicle.map = T2kSpriteEngine(FEngine).mapObj.id);
   FAnimated := false;
   FAltitude := whichVehicle.template.altitude;
   if FAltitude = 0 then
      FUnloadMethod := um_front
   else FUnloadMethod := um_here;
   FShadow := TSprite.Create(FTiles[1]);
//   FShadow.ImageName := 'SysShadow';
   FShadow.Alpha := 160;
   FShadow.Z := 1;
   FShadow.Visible := false;
   FOnCleanup := cleanup;
   FMoveFreq := 8;
end;

destructor TVehicleSprite.Destroy;
begin
   FMapObj.free;
   if assigned(FOnCleanup) then
      FOnCleanup();
   inherited Destroy;
end;

procedure TVehicleSprite.doAction(const button: TButtonCode);
var
   ground: TMapTile;
begin
   if FAltitude = 0 then
   begin
      ground := T2kSpriteEngine(FEngine)[0, unloadLocation.x, unloadLocation.y];
      if (button = btn_enter) and (not ground.open(nil)) then
      begin
         self.activateEvents(ground);
         Exit;
      end;
   end;

   case button of
      btn_enter: self.state := vs_landing;
      btn_cancel: ;
      btn_up, btn_down, btn_left, btn_right: ;
      else assert(false);
   end;
end;

destructor TVehicleTile.destroy;
begin
   if assigned(FOnCleanup) then
      FOnCleanup();
   inherited Destroy;
end;

procedure TVehicleTile.Draw;
begin
   case FOwner.FState of
      vs_empty, vs_emptying, vs_active: ;
      vs_launching: offsetTowards(sgPoint(0, -FOwner.FAltitude), vs_active);
      vs_landing: offsetTowards(commons.origin, vs_emptying);
      else assert(false);
   end;
   inherited Draw;
end;

procedure TVehicleSprite.launch;
begin
   self.state := vs_launching;
   FTiles[1].Z := 13;
   FTiles[2].Z := 13;
   FAnimated := true;

   if FTemplate.template.movementStyle <> msSurface then
   begin
      if self.facing = facing_up then
         self.facing := facing_right
      else if self.facing = facing_down then
         self.facing := facing_left;
      FShadow.visible := true;
      FShadow.x := FTiles[1].x + 4;
      FShadow.y := FTiles[1].y;
      FShadow.Z := 13;
   end;
end;

procedure TVehicleSprite.place;
var
   kept: integer;
begin
   kept := FMoveFrame;
   inherited place;
   FMoveFrame := kept;
   inc(FAnimCounter);
   if FAnimCounter = VEHICLE_ANIM_RATE then
   begin
      FAnimCounter := 0;
      if FAnimated then
         turbu_sprites.nextPosition(FActionMatrix, FAction, FMoveFrame);
   end;
   if (GEnvironment.Party.base = self) and (FMoved) and (not GSpriteEngine.screenLocked) then
      T2kSpriteEngine(FEngine).moveTo(trunc(FTiles[1].X + GSpriteEngine.DisplacementX),
                               trunc(FTiles[1].Y + GSpriteEngine.Displacementy));

   if FMoved and (FTemplate.template.movementStyle <> msSurface) then
   begin
      case self.facing of
         facing_up: FShadow.Y := FShadow.Y - 4;
         facing_right: FShadow.x := FShadow.x + 4;
         facing_down: FShadow.Y := FShadow.Y + 4;
         facing_left: FShadow.x := FShadow.x - 4;
      end;
   end;

end;

procedure TVehicleSprite.reportState(which: TVehicleState);
begin
   inc(FStateCounter);
   if FStateCounter = 2 then
   begin
      self.state := which;
      FStateCounter := 0;
   end;
end;

procedure TVehicleSprite.setState(const Value: TVehicleState);
begin
   FState := Value;
   if FState = vs_emptying then
      self.unload;
end;

procedure TVehicleSprite.SetTarget(const value: TsgPoint);
begin
   if FState = vs_active then
      FTarget := value + TVehicleTile(FTiles[1]).FOffset
   else inherited SetTarget(value);
end;

function TVehicleSprite.unload: boolean;
var
   ground: TTile;
begin
   result := false;
   ground := T2kSpriteEngine(FEngine)[0, unloadLocation.x, unloadLocation.y];
   if ground.open(self) then
   begin
      result := true;
      self.state := vs_empty;
      FTemplate.carrying := nil;
      FCarrying.location := self.unloadLocation;
      FCarrying.visible := true;
      T2kSpriteEngine(FEngine).currentParty := FCarrying;
      FShadow.Visible := false;
      FTiles[1].Z := 3;
      FTiles[2].Z := 3;
   end else self.state := vs_launching;
end;

function TVehicleSprite.unloadLocation: TSgPoint;
begin
   case FUnloadMethod of
      um_here: result := FLocation;
      um_front: result := self.inFront;
   end;
end;

procedure TVehicleSprite.action(const button: TButtonCode);
begin
   if FState = vs_active then
      doAction(button);
end;

procedure TVehicleSprite.bump(bumper: TMapSprite);
begin
   if bumper is THeroSprite then
      THeroSprite(bumper).boardVehicle;
end;

function TVehicleSprite.canMoveForward: boolean;
var
   engine: T2kSpriteEngine;
   sprite: TMapSprite;
   terrain: TRpgTerrain;
begin
   engine := FEngine as T2kSpriteEngine;
   result := engine.edgeCheck(FLocation.X, FLocation.Y, self.facing);
   if result then
   begin
      terrain := GDatabase.terrains[engine[0, inFront.x, inFront.y].terrain];
      result := terrain.vehiclePass[FTemplate.vehicleIndex];
   end;
   if result and (FAltitude = 0) then
   begin
      for sprite in (inFrontTile as TMapTile).event do
      begin
         if ((sprite is TEventSprite) and
            (TEventSprite(sprite).baseTile.Z = 4)) then //second check
            result := false
         else if (sprite is TCharSprite) and not (sprite is TVehicleSprite) then
            result := false;
      end;
   end;
end;

procedure TVehicleSprite.setLocation(data: TSgPoint);
begin
   inherited setLocation(data);
   place;
   if assigned(FShadow) then
   begin
      FShadow.x := FTiles[1].x + 4;
      FShadow.Y := FTiles[1].y;
   end;
   FTiles[1].x := FTiles[1].x + TVehicleTile(FTiles[1]).FOffset.x;
   FTiles[1].y := FTiles[1].y + TVehicleTile(FTiles[1]).FOffset.y;
   FTiles[2].x := FTiles[2].x + TVehicleTile(FTiles[2]).FOffset.x;
   FTiles[2].y := FTiles[2].y + TVehicleTile(FTiles[2]).FOffset.y;
end;

{ THeroSprite }

constructor THeroSprite.create(const AParent: TSpriteEngine; whichHero: TRpgHero; party: TRpgParty);
const x = 1; y = 1;
begin
   inherited create(nil, AParent);
   FTiles[2].Z := 5;
   FTiles[1].Z := 4;
   party.SetSprite(self);
   self.OnChangeSprite := party.ChangeSprite;
   FTemplate := whichHero;
   if assigned(FTemplate) and (FTemplate.sprite <> '') then
      update(FTemplate.sprite, FTemplate.transparent);
   setLocation(point(x, y));
   FParty := party;
   FMoveFreq := 8;
   FCanSkip := true;
   self.facing := facing_down;
end;

destructor THeroSprite.Destroy;
begin
   if FParty.Sprite = self then
      FParty.SetSprite(nil);
   inherited Destroy;;
end;

procedure THeroSprite.action(const button: TButtonCode = btn_enter);
var
   currentTile: TMapTile;
   location: TSgPoint;
begin
   case button of
      btn_enter:
      begin
         activateEvents(TMapTile(T2kSpriteEngine(FEngine)[0, FLocation.x, FLocation.y]));
         currentTile := TMapTile(self.inFrontTile);
         if assigned(currentTile) then
         begin
            activateEvents(currentTile);
            location := FLocation;
            while currentTile.countertop do
            begin
               currentTile := GSpriteEngine.tileInFrontOf(location, self.facing);
               if assigned(currentTile) then
                  activateEvents(currentTile)
               else Break;
            end;
         end;
      end;
      btn_cancel: ; //update this later
   end;
end;

procedure THeroSprite.rideVehicle(theVehicle: TVehicleSprite);
begin
   T2kSpriteEngine(FEngine).currentParty := theVehicle;
   theVehicle.FTemplate.carrying := FParty;
   theVehicle.FCarrying := self;
   self.leaveTile;
   FLocation := point(-1, -1);
   self.visible := false;
   theVehicle.launch;
end;

procedure THeroSprite.boardVehicle;
var
   eventList: TArray<TMapSprite>;
   theSprite: TMapSprite;
begin
   eventList := TMapTile(T2kSpriteEngine(FEngine)[0, FLocation.x, FLocation.y]).event;
   for theSprite in eventlist do
   begin
      if (theSprite is TVehicleSprite) and (TVehicleSprite(theSprite).state = vs_empty) then
      begin
         rideVehicle(TVehicleSprite(theSprite));
         Exit;
      end;
   end;

   case self.facing of
      facing_up: if FLocation.y = 0 then Exit;
      facing_right: if FLocation.X = T2kSpriteEngine(FEngine).width then Exit;
      facing_down: if FLocation.Y = T2kSpriteEngine(FEngine).height then Exit;
      facing_left: if FLocation.x = 0 then Exit;
   end;

   eventList := TMapTile(T2kSpriteEngine(FEngine)[0, inFront.x, inFront.y]).event;
   for theSprite in eventlist do
   begin
      if (theSprite is TVehicleSprite) and (TVehicleSprite(theSprite).state = vs_empty) then
         rideVehicle(TVehicleSprite(theSprite));
   end;
end;

function THeroSprite.doMove(which: TPath): boolean;
begin
   FMoveTick := true;
   result := inherited doMove(which);
   FMoveTick := false;
end;

function THeroSprite.getCanSkip: boolean;
begin
   result := true;
end;

function THeroSprite.move(whichDir: TFacing): boolean;
begin
   result := false;
   if assigned(FMoveTime) then
   begin
      if (moveFreq = 8) and (FMoveTime.timeRemaining <= TRpgTimestamp.FrameLength) then
         queueMove(whichDir);
      Exit;
   end;
   if assigned(FPause) then
   begin
      if FPause.timeRemaining <= TRpgTimestamp.FrameLength then
         queueMove(whichDir);
      Exit;
   end;

   if FMoveTick then
      result := inherited move(whichDir)
   else setMovement(whichDir);
end;

procedure THeroSprite.packUp;
begin
   FEngine.Remove(FTiles[1]);
   FEngine.Remove(FTiles[2]);
   (FEngine as T2kSpriteEngine).LeaveLocation(FLocation, self);
end;

procedure THeroSprite.setMovement(direction: TFacing);
begin
   if not assigned(MoveQueue) then
      MoveQueue := TPath.Create(direction)
   else MoveQueue.setDirection(direction);
end;

procedure THeroSprite.settleDown(engine: TSpriteEngine);
begin
   FEngine := engine;
end;

procedure THeroSprite.place;
begin
   inherited place;
   if FMoveQueued then
   begin
      FMoveQueued := false;
      if moveAssign = nil then
         self.move(FNextMove);
   end;
end;

procedure THeroSprite.queueMove(direction: TFacing);
begin
   FNextMove := direction;
   FMoveQueued := true;
end;

end.
