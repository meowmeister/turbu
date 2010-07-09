unit turbu_2k_char_sprites;

interface
uses
   types,
   commons, tiles, turbu_2k_map_tiles, turbu_map_sprites, turbu_mapchars,
   turbu_defs, turbu_heroes, turbu_pathing,
   sdl_sprite, SG_defs;

type
   TVehicleState = (vs_empty, vs_launching, vs_active, vs_landing, vs_emptying);

   TVehicleSprite = class;

   TVehicleTile = class(TEventTile)
   protected
      FState: TVehicleState;
      FOwner: TVehicleSprite;
      FOffset: TPoint;

      procedure offsetTowards(const offset: TPoint; const state: TVehicleState);
   public
      constructor Create(const base: TEventTile; parent: TSpriteEngine); reintroduce;
   end;

   THeroSprite = class;

   TTileClass = class of TVehicleTile;

   TUnloadMethod = (um_here, um_front);

   TVehicleSprite = class abstract(TCharSprite)
   private
      FTemplate: TRpgVehicle;
      FCarrying: THeroSprite;
      FStateCounter: byte;

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
      function unload: boolean; virtual;
      procedure doAction(const button: TButtonCode); virtual;
   public
      constructor Create(parent: TSpriteEngine; whichVehicle: TRpgVehicle; tileClass: TTileClass); reintroduce; virtual;
      destructor Destroy; override;
      procedure launch; virtual;
      procedure place; override;
      procedure action(const button: TButtonCode = btn_enter); override; final;

      property state: TVehicleState read FState write setState;
      property template: TRpgVehicle read FTemplate;
   end;

   TGroundVehicleSprite = class(TVehicleSprite)
   protected
      function canMoveForward: boolean; override;
      procedure doAction(const button: TButtonCode = btn_enter); override;
   public
      constructor Create(parent: TSpriteEngine; whichVehicle: TRpgVehicle; tileClass: TTileClass); override;
      procedure launch; override;
   end;

   TAirshipSprite = class(TVehicleSprite)
   private
      FShadow: TMiniTile;
   protected
      function unload: boolean; override;
   public
      constructor Create(parent: TSpriteEngine; whichVehicle: TRpgVehicle; tileClass: TTileClass); override;
      destructor Destroy; override;
      procedure launch; override;
      procedure place; override;
      procedure setLocation(data: TSgPoint); override;
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
      procedure action(const button: TButtonCode = btn_enter); override;
      procedure place; override;
      procedure boardVehicle;
      function move(whichDir: TFacing): boolean; override;
      procedure packUp;
      procedure settleDown;

      property template: TRpgHero read FTemplate;
   end;

implementation
uses
   charset_data, turbu_2k_sprite_engine, timing;

const
   AIRSHIP_OFFSET: TPoint = (x: 0; y: -16);
   VEHICLE_ANIM_RATE = 10;

type
   TGroundVehicleTile = class(TVehicleTile)
   public
      procedure Draw; override;
   end;

   TAirshipTile = class(TVehicleTile)
   public
      procedure Draw; override;
   end;

{ TVehicleTile }

constructor TVehicleTile.Create(const base: TEventTile; parent: TSpriteEngine);
begin
   inherited Create(base.event, parent as T2kSpriteEngine);
   self.z := base.z;
   self.Assign(base);
end;

procedure TVehicleTile.offsetTowards(const offset: TPoint; const state: TVehicleState);
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
   //end if
end;

{ TVehicleSprite }

procedure TVehicleSprite.action(const button: TButtonCode);
begin
   if FState = vs_active then
      doAction(button);
end;

function TVehicleSprite.canMoveForward: boolean;
begin
   result := T2kSpriteEngine(FEngine).edgeCheck(FLocation.X, FLocation.Y, self.facing){ and
      GDatabase.terrain[T2kSpriteEngine(FEngine)[lower, inFront.x, inFront.y].terrain].vehiclePass[FTemplate.vehicleType]};
end;

constructor TVehicleSprite.Create(parent: TSpriteEngine; whichVehicle: TRpgVehicle; tileClass: TTileClass);
var dummy: TVehicleTile;
begin
   inherited Create(nil, parent, whichvehicle);
   whichVehicle.gamesprite := self;
   self.FTemplate := whichVehicle;
   //update char tiles to vehicle tiles
   dummy := tileClass.Create(FTiles[2] as TEventTile, parent);
   dummy.FOwner := self;
   FTiles[2].Free;
   FTiles[2] := dummy;
   dummy := tileClass.Create(FTiles[1] as TEventTile, parent);
   dummy.FOwner := self;
   FTiles[1].Free;
   FTiles[1] := dummy;
   visible := (whichVehicle.map = T2kSpriteEngine(FEngine).mapObj.id);
//   FMapObj := TEvent.create(SCRIPT_HEADER + 'rideVehicle;' + SCRIPT_FOOTER);
   FAnimated := true;
end;

destructor TVehicleSprite.Destroy;
begin
   FMapObj.free;
   inherited Destroy;
end;

procedure TVehicleSprite.doAction(const button: TButtonCode);
begin
   case button of
      btn_enter: self.state := vs_landing;
      btn_cancel: ;
      btn_up, btn_down, btn_left, btn_right: ;
      else assert(false);
   end;
end;

procedure TVehicleSprite.launch;
begin
   self.state := vs_launching;
   FTiles[1].Z := 13;
   FTiles[2].Z := 13;
end;

procedure TVehicleSprite.place;
var kept: word;
begin
   kept := FMoving;
   inherited place;
   FMoving := kept;
   inc(FAnimCounter);
{$MESSAGE WARN 'Commented out code in live unit'}
{   if (FAnimated) and (FAnimCounter = VEHICLE_ANIM_RATE) then
   begin
      if (FAnimDir = true) and (animFrame = high(TAnimFrame)) then //counting up
         FAnimDir := false
      else if (FAnimDir = false) and (animFrame = low(TAnimFrame)) then
         FAnimDir := true;

      if FanimDir then
         animFrame := TAnimFrame(ord(animFrame) + 1)
      else
         animFrame := TAnimFrame(ord(animFrame) - 1);
      //end if
   end;
   if FAnimCounter = VEHICLE_ANIM_RATE then
      FAnimCounter := 0;
   if (GGameEngine.currentParty = self) and (FMoved) and (not GGameEngine.screenLocked) then
      T2kSpriteEngine(FEngine).moveTo(trunc(FTiles[1].X) + GGameEngine.displacement.x,
                               trunc(FTiles[1].Y) + GGameEngine.displacement.y);}
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
   TVehicleTile(FTiles[1]).FState := value;
   TVehicleTile(FTiles[2]).FState := value;
   if FState = vs_emptying then
      self.unload;
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
   end else self.state := vs_launching;
end;

function TVehicleSprite.unloadLocation: TSgPoint;
begin
   case FUnloadMethod of
      um_here: result := FLocation;
      um_front: result := self.inFront;
   end;
end;

{ TAirshipSprite }

constructor TAirshipSprite.Create(parent: TSpriteEngine; whichVehicle: TRpgVehicle; tileClass: TTileClass);
begin
   inherited Create(parent, whichVehicle, TAirshipTile);
//   FShadow := TMiniTile.Create(parent, nil);
   FShadow.ImageName := 'SysShadow';
   FShadow.Alpha := 160;
   FShadow.Visible := false;
   FAnimated := false;
   FTiles[2].Z := 3;
   FTiles[1].Z := 3;
end;

destructor TAirshipSprite.Destroy;
begin
   FShadow.Free;
   inherited;
end;

procedure TAirshipSprite.launch;
begin
   inherited launch;
   FAnimated := true;
   if self.facing = facing_up then
      self.facing := facing_right
   else if self.facing = facing_down then
      self.facing := facing_left;
   FShadow.visible := true;
   FShadow.x := FTiles[1].x + 4;
   FShadow.y := FTiles[1].y;
   FShadow.Z := 13;
end;

procedure TAirshipSprite.place;
begin
   inherited place;
   if FMoved then
   begin
      case self.facing of
         facing_up: FShadow.Y := FShadow.Y - 4;
         facing_right: FShadow.x := FShadow.x + 4;
         facing_down: FShadow.Y := FShadow.Y + 4;
         facing_left: FShadow.x := FShadow.x - 4;
      end;
   end;
end;

procedure TAirshipSprite.setLocation(data: TSgPoint);
begin
   inherited setLocation(data);
   if assigned(FShadow) then
   begin
      FShadow.x := FTiles[1].x + 4;
      FShadow.Y := FTiles[1].y;
   end;
   FTiles[1].x := FTiles[1].x + TAirshipTile(FTiles[1]).FOffset.x;
   FTiles[1].y := FTiles[1].y + TAirshipTile(FTiles[1]).FOffset.y;
   FTiles[2].x := FTiles[2].x + TAirshipTile(FTiles[2]).FOffset.x;
   FTiles[2].y := FTiles[2].y + TAirshipTile(FTiles[2]).FOffset.y;
end;

{$WARN NO_RETVAL OFF}
function TAirshipSprite.unload: boolean;
begin
   if inherited unload then
   begin
      FShadow.Visible := false;
      FTiles[1].Z := 3;
      FTiles[2].Z := 3;
   end;
end;
{$WARN NO_RETVAL ON}

{ TGroundVehicleSprite }

constructor TGroundVehicleSprite.Create(parent: TSpriteEngine;
  whichVehicle: TRpgVehicle; tileClass: TTileClass);
begin
   inherited Create(parent, whichVehicle, TGroundVehicleTile);
   FUnloadMethod := um_front;
end;

procedure TGroundVehicleSprite.doAction(const button: TButtonCode);
var
   ground: TMapTile;
begin
   ground := T2kSpriteEngine(FEngine)[0, unloadLocation.x, unloadLocation.y];
   if (button = btn_enter) and (not ground.open(nil)) then
      self.activateEvents(ground)
   else inherited doAction(button);
end;

function TGroundVehicleSprite.canMoveForward: boolean;
var
  sprite: TMapSprite;
begin
   result := inherited canMoveForward;
   if result then
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

procedure TGroundVehicleSprite.launch;
begin
   inherited launch;
   FState := vs_active;
end;

{ THeroSprite }

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
{            while currentTile.countertop do
            begin
               currentTile := GGameEngine.tileInFrontOf(location, self.facing);
               if assigned(currentTile) then
                  activateEvents(currentTile)
               else Break;
            end; }
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

constructor THeroSprite.create(const AParent: TSpriteEngine; whichHero: TRpgHero; party: TRpgParty);
const x = 1; y = 1;
begin
   inherited create(nil, AParent, party);
   FTemplate := whichHero;
   if assigned(FTemplate) and (FTemplate.sprite <> '') then
      update(FTemplate.sprite, FTemplate.transparent);
   setLocation(point(x, y));
   FParty := party;
   moveFreq := 8;
   FCanSkip := true;
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
   else
      setMovement(whichDir);
end;

procedure THeroSprite.packUp;
begin
   FEngine.Remove(FTiles[1]);
   FEngine.Remove(FTiles[2]);
//   T2kSpriteEngine(FEngine).character[0] := nil;
end;

procedure THeroSprite.setMovement(direction: TFacing);
begin
   if not assigned(MoveQueue) then
      MoveQueue := TPath.Create(direction)
   else MoveQueue.setDirection(direction);
end;

procedure THeroSprite.settleDown;
begin
   FEngine.add(FTiles[1]);
   FEngine.add(FTiles[2]);
end;

procedure THeroSprite.place;
begin
   inherited place;
   if FMoveQueued then
   begin
      FMoveQueued := false;
      self.move(FNextMove);
   end;
end;

procedure THeroSprite.queueMove(direction: TFacing);
begin
   FNextMove := direction;
   FMoveQueued := true;
end;

{ TGroundVehicleTile }

procedure TGroundVehicleTile.Draw;
begin
   inherited Draw;
   if FState = vs_landing then
      FOwner.reportState(vs_emptying);
end;

{ TAirshipTile }

procedure TAirshipTile.Draw;
begin
   case FState of
      vs_empty, vs_emptying, vs_active: ;
      vs_launching: offsetTowards(AIRSHIP_OFFSET, vs_active);
      vs_landing: offsetTowards(commons.origin, vs_emptying);
      else assert(false);
   end;
   inherited Draw;
end;

end.
