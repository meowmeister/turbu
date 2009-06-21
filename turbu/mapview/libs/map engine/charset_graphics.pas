unit charset_graphics;
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
   types, //windows libs
   commons, charset_data, chipset_data, tiles, hero_data, events, script_backend,
   script_interface, move_data, addition_sprite,//turbu libs
   {AsphyreSprite} SG_Defs, SDL_sprite; //asphyre libs

type
   TEventSprite = class(TAdditionSprite)
   protected
      procedure setLocation(data: TSgPoint); override;
   public
      constructor create(baseEvent: TEvent; const AParent: TSpriteEngine; char: TRpgCharacter); override;
      procedure updatePage(data: TEventPage); override;
      procedure update(filename: string; index: byte; transparent: boolean); override;
   end;

   TCharTile = class(TTile)
   private
      FWhichChar: byte;
      FPatternIndex: integer;
   protected
      procedure SetPatternIndex(const Value: Integer); override;
   public
      property whichChar: byte read FWhichChar write FWhichChar;
      property patternIndex: integer read FPatternIndex write setPatternIndex;
   end;

   TCharSprite = class(TAdditionSprite)
   private
      FAnimFrame: TAnimFrame;

      procedure setAnimFrame(data: TAnimFrame);
      procedure loadCharset(filename: string);
      function workOutAnimFrame: TAnimFrame;
   protected
      FMoved: boolean;
      procedure setFacing(data: TFacing); override;
      procedure updateTiles;
      procedure setLocation(data: TSgPoint); override;
      procedure setTranslucency(const value: byte); override;
      procedure activateEvents(where: TLowerTile);
   public
      constructor create(baseEvent: TEvent; const AParent: TSpriteEngine; char: TRpgCharacter); override;
      procedure reload(const imageName: string; const index: byte);
      procedure assign(data: TCharSprite); reintroduce;
      procedure place; override;
      procedure update(filename: string; index: byte; transparent: boolean); override;
      procedure updatePage(data: TEventPage); override;
      procedure action(const button: TButtonCode = btn_enter); virtual; abstract;

      property frame: smallint read FWhichFrame;
      property animFrame: TAnimFrame read FAnimFrame write setAnimFrame;
   end;

   TVehicleState = (vs_empty, vs_launching, vs_active, vs_landing, vs_emptying);

   TVehicleSprite = class;

   TVehicleTile = class(TCharTile)
   protected
      FState: TVehicleState;
      FOwner: TVehicleSprite;
      FOffset: TPoint;

      procedure offsetTowards(const offset: TPoint; const state: TVehicleState);
   public
      constructor Create(const base: TCharTile; parent: TSpriteEngine); reintroduce;
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
      procedure launch; override;
   end;

   TBoatSprite = class(TGroundVehicleSprite)
   public
      constructor Create(parent: TSpriteEngine; whichVehicle: TRpgVehicle; tileClass: TTileClass); override;
   end;

   TShipSprite = class(TGroundVehicleSprite)
   public
      constructor Create(parent: TSpriteEngine; whichVehicle: TRpgVehicle; tileClass: TTileClass); override;
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
   protected
      function doMove(which: TMoveOrder): boolean; override;
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

const
   AIRSHIP_OFFSET: TPoint = (x: 0; y: -16);
   VEHICLE_ANIM_RATE = 10;

implementation
uses sysUtils, classes, contnrs,
     chipset_graphics, locate_files, LMU, script_engine, LDB, rs_map,
     {asphyredef} turbu_defs;

type
   TBoatTile = class(TVehicleTile)
   public
      procedure Draw; override;
   end;

   TShipTile = class(TVehicleTile)
   public
      procedure Draw; override;
   end;

   TAirshipTile = class(TVehicleTile)
   public
      procedure Draw; override;
   end;

{CHARACTER TILES}
{$REGION CHAR TILES}
{ TCharTile }

procedure TCharTile.SetPatternIndex(const Value: Integer);
var
   dummy: integer;
begin
   FPatternIndex := value;

   dummy := value mod 3; //isolate X position
   if FWhichChar > 3 then //check which half of the sprite sheet
      inc(dummy, 96);
   inc(dummy, (FWhichChar mod 4) * 3);
   inc(dummy, (value div 3) * 12);
   inherited setPatternIndex(dummy);
end;

{ TVehicleTile }

constructor TVehicleTile.Create(const base: TCharTile; parent: TSpriteEngine);
begin
   inherited Create(parent as TGameMap, nil);
   Self.FWhichChar := base.FWhichChar;
   self.FPatternIndex := base.FPatternIndex;
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

{ TBoatTile }

procedure TBoatTile.Draw;
begin
   inherited Draw;
   if FState = vs_landing then
      FOwner.reportState(vs_emptying);
   //end if
end;

{ TShipTile }

procedure TShipTile.Draw;
begin
   inherited Draw;
   if FState = vs_landing then
      FOwner.reportState(vs_emptying);
   //end if
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
{$ENDREGION}

{ TEventSprite }

constructor TEventSprite.create(baseEvent: TEvent; const AParent: TSpriteEngine; char: TRpgCharacter);
begin
   inherited Create(baseEvent, AParent, char);
   FTiles[bottom] := TEventTile.create(baseEvent, TGameMap(AParent), GGameEngine.currentMap.template);
   FTiles[top] := nil;
   if assigned(FEvent.lastCurrentPage) and FEvent.lastCurrentPage.transparent then
      translucency := 3
   else translucency := 0;
   setLocation(point(baseEvent.location.X, baseEvent.location.Y));
end;

procedure TEventSprite.setLocation(data: TSgPoint);
begin
   inherited setLocation(data);
   FTiles[bottom].X := location.X * TILESIZE;
   FTiles[bottom].Y := location.Y * TILESIZE;
end;

procedure TEventSprite.update(filename: string; index: byte; transparent: boolean);
var orphan: TRpgEvent;
begin
   self.translucency := 3 * ord(transparent);
   FEvent.lastCurrentPage.overrideSprite(filename, index, transparent);
   orphan := FCharacter as TRpgEvent;
   orphan.switchType;
   orphan.changeSprite(filename, index);
end;

procedure TEventSprite.updatePage(data: TEventPage);
begin
   TEventTile(FTiles[bottom]).update(data);
end;

{ TCharSprite }

procedure TCharSprite.activateEvents(where: TLowerTile);
var
   eventList: TObjectList;
   i: integer;
   eventPtr: TEvent;
begin
   eventList := where.event;
   for i := 0 to eventlist.Count - 1 do
   begin
      eventPtr := (eventList[i] as TAdditionSprite).event;
      if (eventList[i] <> self) and assigned(eventPtr.lastCurrentPage) and (eventPtr.lastCurrentPage.hasScript)
         and (eventPtr.lastCurrentPage.startCondition = by_key) then
         GScriptEngine.executeEvent(eventPtr, eventList[i] as TAdditionSprite);
      //end if
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

constructor TCharSprite.create(baseEvent: TEvent; const AParent: TSpriteEngine; char: TRpgCharacter);
begin
   inherited Create(baseEvent, AParent, char);
   FUnderConstruction := true;
   FTiles[bottom] := TCharTile.Create(TGameMap(AParent), nil);
   FTiles[top] := TCharTile.Create(TGameMap(AParent), nil);
   if baseEvent <> nil then
   begin
      assert(baseEvent.lastCurrentPage <> nil);
      if baseEvent.lastCurrentPage.transparent then
         translucency := 3;
      self.facing := baseEvent.lastCurrentPage.direction;
      updatePage(baseEvent.lastCurrentPage);
      setLocation(point(baseEvent.location.X, baseEvent.location.Y));
   end;
   FTiles[top].Z := 5;
   FTiles[bottom].Z := 4;
   FUnderConstruction := false;   
end;

procedure TCharSprite.place;
begin
   FMoved := (FMoving > 0) and (FWhichFrame = FMoveRate - 1);
   inherited place;
   self.animFrame := self.workOutAnimFrame;
   FTiles[top].Y := FTiles[bottom].Y - TILESIZE;
   FTiles[top].X := FTiles[bottom].X;
end;

procedure TCharSprite.updateTiles;
begin
   FTiles[top].ImageIndex := ord(self.facing) * 6 + ord(FAnimFrame);
   FTiles[bottom].ImageIndex := TCharTile(FTiles[top]).ImageIndex + 3;
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
         case FEvent.lastCurrentPage.animType of
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

procedure TCharSprite.loadCharset(filename: string);
var oldName: string;
begin
   oldName := filename;
   findGraphic(filename, 'charset');
{   if filename = '' then
      raise EParseMessage.create('Charset graphic file "' + fileName + '" not found!');}
   if filename <> '' then
      TGameMap(FEngine).loadCharset(oldName, filename);
end;

procedure TCharSprite.reload(const imageName: string; const index: byte);
begin
   FTiles[bottom].name := imagename + intToStr(index);
   FTiles[top].name := imagename + intToStr(index);
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
   FTiles[bottom].X := (location.X * TILESIZE) - 4;
   FTiles[bottom].Y := location.Y * TILESIZE;
   FTiles[top].X := (location.X * TILESIZE) - 4; //correction for char width
   FTiles[top].Y := (location.Y - 1) * TILESIZE; //correction for double-height
end;

procedure TCharSprite.setTranslucency(const value: byte);
begin
   inherited setTranslucency(value);
   FTiles[top].Alpha := FTiles[bottom].Alpha;
end;

procedure TCharSprite.update(filename: string; index: byte; transparent: boolean);
begin
   loadCharset(filename);
   FTiles[top].ImageName := 'Charset ' + filename;
   FTiles[bottom].ImageName := 'Charset ' + filename;
   TCharTile(FTiles[top]).whichChar := index;
   TCharTile(FTiles[bottom]).whichChar := index;
   if transparent then
      self.translucency := 3
   else self.translucency := 0;
   updateTiles;
end;

procedure TCharSprite.updatePage(data: TEventPage);
var index: byte;
begin
   index := data.whichChip;
   FUnderConstruction := true;
   self.facing := data.direction;
   FUnderConstruction := false;
   update(data.filename, index, translucency >= 3);
   FTiles[top].ImageIndex := ord(data.direction) * 6 + ord(data.animFrame);
   FTiles[bottom].ImageIndex := TCharTile(FTiles[top]).ImageIndex + 3;
end;

{ THeroSprite }

procedure THeroSprite.action(const button: TButtonCode = btn_enter);
var
   currentTile: TLowerTile;
   location: TSgPoint;
begin
   case button of
      btn_enter:
      begin
         activateEvents(TLowerTile(TGameMap(FEngine)[lower, FLocation.x, FLocation.y]));
         currentTile := TLowerTile(self.inFrontTile);
         if assigned(currentTile) then
         begin
            activateEvents(currentTile);
            location := FLocation;
            while currentTile.countertop do
            begin
               currentTile := GGameEngine.tileInFrontOf(location, self.facing);
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
   TGameMap(FEngine).currentParty := theVehicle;
   theVehicle.FTemplate.carrying := FParty;
   theVehicle.FCarrying := self;
   self.leaveTile;
   FLocation := point(-1, -1);
   self.visible := false;
   theVehicle.launch;
end;

procedure THeroSprite.boardVehicle;
var
   i: integer;
   eventList: TObjectList;
   theSprite: TAdditionSprite;
begin
   eventList := TLowerTile(TGameMap(FEngine)[lower, FLocation.x, FLocation.y]).event;
   for i := 0 to eventlist.Count - 1 do
   begin
      theSprite := eventList[i] as TAdditionSprite;
      if (theSprite is TVehicleSprite) and (TVehicleSprite(theSprite).state = vs_empty) then
      begin
         rideVehicle(TVehicleSprite(theSprite));
         Exit;
      end;
   end;

   case self.facing of
      facing_up: if FLocation.y = 0 then Exit;
      facing_right: if FLocation.X = TGameMap(FEngine).width then Exit;
      facing_down: if FLocation.Y = TGameMap(FEngine).height then Exit;
      facing_left: if FLocation.x = 0 then Exit;
   end;

   eventList := TLowerTile(TGameMap(FEngine)[lower, inFront.x, inFront.y]).event;
   for i := 0 to eventlist.Count - 1 do
   begin
      theSprite := eventList[i] as TAdditionSprite;
      if (theSprite is TVehicleSprite) and (TVehicleSprite(theSprite).state = vs_empty) then
         rideVehicle(TVehicleSprite(theSprite));
      //end if
   end;
end;

constructor THeroSprite.create(const AParent: TSpriteEngine; whichHero: TRpgHero; party: TRpgParty);
const x = 1; y = 1;
begin
   inherited create(nil, AParent, party);
   FTemplate := whichHero;
   if assigned(FTemplate) then
      update(FTemplate.sprite, FTemplate.spriteIndex, FTemplate.transparent);
   setLocation(point(x, y));
   FParty := party;
   moveFreq := 8;
   FCanSkip := true;
end;

function THeroSprite.doMove(which: TMoveOrder): boolean;
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
      if (moveFreq = 8) and (FMoveTime.timeRemaining <= GFrameLength) then
         queueMove(whichDir);
      Exit;
   end;
   if assigned(FPause) then
   begin
      if FPause.timeRemaining <= GFrameLength then
         queueMove(whichDir);
      Exit;
   end;

   if FMoveTick then
      result := inherited move(whichDir)
   else
      setMovement(whichDir);
   //end if
end;

procedure THeroSprite.packUp;
begin
   FEngine.Remove(FTiles[bottom]);
   FEngine.Remove(FTiles[top]);
   TGameMap(FEngine).character[0] := nil;
end;

procedure THeroSprite.settleDown;
begin
   FEngine.add(FTiles[bottom]);
   FEngine.add(FTiles[top]);
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

{ TVehicleSprite }

procedure TVehicleSprite.action(const button: TButtonCode);
begin
   if FState = vs_active then
      doAction(button);
end;

function TVehicleSprite.canMoveForward: boolean;
begin
   result := TGameMap(FEngine).edgeCheck(FLocation.X, FLocation.Y, self.facing){ and
      GDatabase.terrain[TGameMap(FEngine)[lower, inFront.x, inFront.y].terrain].vehiclePass[FTemplate.vehicleType]};
end;

constructor TVehicleSprite.Create(parent: TSpriteEngine; whichVehicle: TRpgVehicle; tileClass: TTileClass);
var dummy: TVehicleTile;
begin
   inherited Create(nil, parent, whichvehicle);
   whichVehicle.gamesprite := self;
   self.FTemplate := whichVehicle;
   //update char tiles to vehicle tiles
   dummy := tileClass.Create(FTiles[top] as TCharTile, parent);
   dummy.FOwner := self;
   FTiles[top].Free;
   FTiles[top] := dummy;
   dummy := tileClass.Create(FTiles[bottom] as TCharTile, parent);
   dummy.FOwner := self;
   FTiles[bottom].Free;
   FTiles[bottom] := dummy;
   visible := (whichVehicle.map = TGameMap(FEngine).currentMap.mapID);
   FEvent := TEvent.create(SCRIPT_HEADER + 'rideVehicle;' + SCRIPT_FOOTER);
   FAnimated := true;
end;

destructor TVehicleSprite.Destroy;
begin
   FEvent.free;
   inherited;
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
   FTiles[bottom].Z := 13;
   FTiles[top].Z := 13;
end;

procedure TVehicleSprite.place;
var kept: TAnimFrame;
begin
   kept := animFrame;
   inherited place;
   animFrame := kept;
   inc(FAnimCounter);
   if (FAnimated) and (FAnimCounter = VEHICLE_ANIM_RATE) then
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
      TGameMap(FEngine).moveTo(trunc(FTiles[bottom].X) + GGameEngine.displacement.x,
                               trunc(FTiles[bottom].Y) + GGameEngine.displacement.y);
   //end if
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
   TVehicleTile(FTiles[bottom]).FState := value;
   TVehicleTile(FTiles[top]).FState := value;
   if FState = vs_emptying then
      self.unload;
end;

function TVehicleSprite.unload: boolean;
var
   ground: TTile;
begin
   result := false;
   ground := TGameMap(FEngine)[lower, unloadLocation.x, unloadLocation.y];
   if ground.open(self) then
   begin
      result := true;
      self.state := vs_empty;
      FTemplate.carrying := nil;
      FCarrying.location := self.unloadLocation;
      FCarrying.visible := true;
      TGameMap(FEngine).currentParty := FCarrying;
   end else self.state := vs_launching;
end;

function TVehicleSprite.unloadLocation: TSgPoint;
begin
   case FUnloadMethod of
      um_here: result := FLocation;
      um_front: result := self.inFront;
   end;
end;

{ TBoatSprite }

constructor TBoatSprite.Create(parent: TSpriteEngine; whichVehicle: TRpgVehicle; tileClass: TTileClass);
begin
   inherited Create(parent, whichVehicle, TBoatTile);
   FUnloadMethod := um_front;
end;

{ TShipSprite }

constructor TShipSprite.Create(parent: TSpriteEngine; whichVehicle: TRpgVehicle; tileClass: TTileClass);
begin
   inherited Create(parent, whichVehicle, TShipTile);
   FUnloadMethod := um_front;
end;

{ TAirshipSprite }

constructor TAirshipSprite.Create(parent: TSpriteEngine; whichVehicle: TRpgVehicle; tileClass: TTileClass);
begin
   inherited Create(parent, whichVehicle, TAirshipTile);
   FShadow := TMiniTile.Create(parent, nil);
   FShadow.ImageName := 'SysShadow';
   FShadow.Alpha := 160;
   FShadow.Visible := false;
   FAnimated := false;
   FTiles[top].Z := 3;
   FTiles[bottom].Z := 3;
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
   FShadow.x := FTiles[bottom].x + 4;
   FShadow.y := FTiles[bottom].y;
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
      FShadow.x := FTiles[bottom].x + 4;
      FShadow.Y := FTiles[bottom].y;
   end;
   FTiles[bottom].x := FTiles[bottom].x + TAirshipTile(FTiles[bottom]).FOffset.x;
   FTiles[bottom].y := FTiles[bottom].y + TAirshipTile(FTiles[bottom]).FOffset.y;
   FTiles[top].x := FTiles[top].x + TAirshipTile(FTiles[top]).FOffset.x;
   FTiles[top].y := FTiles[top].y + TAirshipTile(FTiles[top]).FOffset.y;
end;

{$WARN NO_RETVAL OFF}
function TAirshipSprite.unload: boolean;
begin
   if inherited unload then
   begin
      FShadow.Visible := false;
      FTiles[bottom].Z := 3;
      FTiles[top].Z := 3;
   end;
end;
{$WARN NO_RETVAL ON}

{ TGroundVehicleSprite }

procedure TGroundVehicleSprite.doAction(const button: TButtonCode);
var
   ground: TTile;
begin
   ground := TGameMap(FEngine)[lower, unloadLocation.x, unloadLocation.y];
   if (button = btn_enter) and (not ground.open(nil)) then
      self.activateEvents(ground as TLowerTile) //yeah, not too type-safe, but I'd rather not
   else inherited doAction(button);
end;

function TGroundVehicleSprite.canMoveForward: boolean;
var
  I: Integer;
  events: TObjectList;
  sprite: TObject;
begin
   result := inherited canMoveForward;
   if result then
   begin
      events := (inFrontTile as TLowerTile).event;
      for I := 0 to events.count - 1 do
      begin
         sprite := events.items[i];
         if ((sprite is TEventSprite) and
            (TEventSprite(sprite).baseTile.Z = 4)) then //second check
            result := false
         else if (sprite is TCharSprite) and not (sprite is TVehicleSprite) then
            result := false;
         //end if
      end;
   end;
end;

procedure TGroundVehicleSprite.launch;
begin
   inherited launch;
   FState := vs_active;
end;

end.
