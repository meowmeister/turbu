unit turbu_mapchars;

interface
uses
   sdl_sprite, sg_defs,
   charset_data,
   turbu_map_sprites, turbu_map_objects, turbu_map_metadata;

type
   TRpgCharacter = class(TInterfacedObject, IRpgCharacter)
   private
      function getScreenX: integer; inline;
      function getScreenY: integer; inline;
      function getScreenXP: integer; inline;
      function getScreenYP: integer; inline;
   protected
      procedure doFlash(r, g, b, power: byte; time: cardinal); virtual; abstract;
      function getX: word; virtual; abstract;
      function getY: word; virtual; abstract;
      function getBase: TMapSprite; virtual; abstract;
      function getTranslucency: byte; virtual;
      procedure setTranslucency(const Value: byte); virtual;
   public
      procedure flash(r, g, b, power: byte; time: cardinal; wait: boolean);
      procedure move(frequency: byte; skip: boolean; route: word);
      procedure ChangeSprite(name: string; index: integer; oldSprite: TMapSprite); virtual; abstract;

      property screenX: integer read getScreenX;
      property screenY: integer read getScreenY;
      property screenXP: integer read getScreenXP;
      property screenYP: integer read getScreenYP;
      property base: TMapSprite read getBase;
      property translucency: byte read getTranslucency write setTranslucency;
   end;

   TRpgEvent = class(TRpgCharacter)
   private
      FID: word;
      FTile: TSprite;
      FBase: TMapSprite;
      FIsChar: boolean;
      FEvent: TRpgMapObject;

      function getLocation: TSgPoint;
      procedure setLocation(const Value: TSgPoint);
      function getMap: word;
      function getFacing: byte;
   protected
      function getX: word; override;
      function getY: word; override;
      function getBase: TMapSprite; override;
      procedure doFlash(r, g, b, power: byte; time: cardinal); override;
   public
      constructor create(base: TMapSprite);
      destructor Destroy; override;
//      destructor delete;
      procedure update;
      procedure ChangeSprite(name: string; index: integer; oldSprite: TMapSprite); override;
      procedure switchType;

      property map: word read getMap;
      property x: word read getX;
      property y: word read getY;
      property facing: byte read getFacing;
      property id: word read FID;
      property base: TMapSprite read FBase;
      property location : TSgPoint read getLocation write setLocation;
   end;

   TRpgVehicle = class(TRpgCharacter)
   private
      FSprite: string;
      FSpriteIndex: byte;
      FMap: smallint;
      FX: word;
      FY: word;
      FGameSprite: TMapSprite;
      FVehicleIndex: integer;
      FCarrying: TRpgCharacter;

      function getLocation: TSgPoint;
      procedure setLocation(const Value: TSgPoint);
      procedure setX(const Value: word);
      procedure setY(const Value: word);
      procedure setMap(const Value: smallint);
      function getFacing: byte;
      procedure setFacing(const Value: byte);
   protected
      function getX: word; override;
      function getY: word; override;
      function getBase: TMapSprite; override;
      procedure doFlash(r, g, b, power: byte; time: cardinal); override;
   public
      constructor Create(mapTree: TMapTree; which: integer);
      destructor Destroy; override;
      procedure setSprite(filename: string; index: byte);
      procedure ChangeSprite(name: string; index: integer; oldSprite: TMapSprite); override;
      function inUse: boolean;

      property sprite: string read FSprite;
      property spriteIndex: byte read FSpriteIndex;
      property map: smallint read FMap write setMap;
      property x: word read getX write setX;
      property y: word read getY write setY;
      property location: TSgPoint read getLocation write setLocation;
      property facing: byte read getFacing write setFacing;
      property gamesprite: TMapSprite read FGameSprite write FGameSprite;
      property vehicleIndex: integer read FVehicleIndex;
      property carrying: TRpgCharacter read FCarrying write FCarrying;
   end;

implementation
uses
   types,
   commons, turbu_defs, turbu_constants, tiles, turbu_2k_char_sprites,
   turbu_2k_sprite_engine;

{ TRpgCharacter }

procedure TRpgCharacter.flash(r, g, b, power: byte; time: cardinal; wait: boolean);
begin
{$MESSAGE WARN 'Commented out code in live unit'}
   doFlash(r, g, b, power, time);
{   if wait then
      TEventThread(GCurrentThread).threadSleep(time, true);}
end;

function TRpgCharacter.getScreenX: integer;
begin
{$MESSAGE WARN 'Commented out code in live unit'}
//   result := getX - round(GCurrentEngine.parent.WorldX / TILESIZE);
end;

function TRpgCharacter.getScreenXP: integer;
begin
   result := self.screenX * TILE_SIZE.x;
end;

function TRpgCharacter.getScreenYP: integer;
begin
   result := self.screenY * TILE_SIZE.y;
end;

function TRpgCharacter.getTranslucency: byte;
begin
   result := base.translucency;
end;

procedure TRpgCharacter.move(frequency: byte; skip: boolean; route: word);
begin
{$MESSAGE WARN 'Commented out code in live unit'}
{   if route > high(GGameEngine.currentMap.routes) then
      Exit;
   if not assigned(self.base) then
      Exit;

   self.base.moveOrder := GGameEngine.currentMap.routes[route];}
   self.base.canSkip := skip;
   self.base.moveFreq := clamp(frequency, 1, 8);
end;

procedure TRpgCharacter.setTranslucency(const Value: byte);
begin
   base.translucency := Value;
end;

function TRpgCharacter.getScreenY: integer;
begin
{$MESSAGE WARN 'Commented out code in live unit'}
//   result := getY - round(GCurrentEngine.parent.WorldY / TILESIZE);
end;

{ TRpgEvent }

constructor TRpgEvent.create(base: TMapSprite);
begin
   FIsChar := (base is TCharSprite);
   FBase := base;
   FBase.character := self;
   FTile := FBase.baseTile;
   FEvent := FBase.event;
   if FEvent <> nil then
      FID := FBase.event.id;
end;

(* destructor TRpgEvent.delete;
var i: smallint;
begin
{   GGameEngine.currentMap.deleteEvent(FBase);
   FBase.leaveTile;
   FBase.Free;
   i := min(FID, high(GRpgEvents));
   while (i >= 0) and (GRpgEvents[i] <> self) do
      dec(i);
   assert(i <> -1);
   for I := i to high(GRpgEvents) - 1 do
      GRpgEvents[i] := GRpgEvents[i + 1];
   setLength(GRpgEvents, length(GRpgEvents) - 1);}
   inherited Destroy;
end; *)

destructor TRpgEvent.Destroy;
begin
{$MESSAGE WARN 'Commented out code in live unit'}
{   if FBase is TNullEvent then
      FBase.free;}
   inherited;
end;

procedure TRpgEvent.ChangeSprite(name: string; index: integer; oldSprite: TMapSprite);
begin
   base.update(name, base.translucency >= 3);
end;

procedure TRpgEvent.doFlash(r, g, b, power: byte; time: cardinal);
begin
   FBase.flash(r, g, b, power, time);
end;

function TRpgEvent.getBase: TMapSprite;
begin
   result := self.FBase;
end;

function TRpgEvent.getFacing: byte;

   function translate(facing: TFacing): byte;
   begin
      case facing of
         facing_up: result := 8;
         facing_right: result := 6;
         facing_down: result := 2;
         facing_left: result := 4;
         else result := 0;
      end;
   end;

begin
   if FIsChar then
      result := translate((FBase as TCharSprite).facing)
   else result := 2;
end;

function TRpgEvent.getLocation: TSgPoint;
begin
   result := point(self.x, self.y);
end;

function TRpgEvent.getMap: word;
begin
   if FBase is TVehicleSprite then
      result := TVehicleSprite(FBase).template.Map
   else
      result := GSpriteEngine.mapID;
end;

function TRpgEvent.getX: word;
begin
   if FIsChar then
      result := (FBase as TCharSprite).location.X
   else with FTile as TEventTile do
      result := trunc(FTile.X) div TILE_SIZE.x;
end;

function TRpgEvent.getY: word;
begin
   if FIsChar then
      result := (FBase as TCharSprite).location.y
   else with FTile as TEventTile do
      result := trunc(FTile.y) div TILE_SIZE.y;
end;

procedure TRpgEvent.setLocation(const Value: TSgPoint);
begin
   FBase.leaveTile;
   FBase.location := value;
end;

{$WARN CONSTRUCTING_ABSTRACT OFF}
procedure TRpgEvent.switchType;
//var index: word;
begin
{$MESSAGE WARN 'Commented out code in live unit'}
{   index := GGameEngine.currentMap.indexOf(FBase);
   FBase.nuke;
   FBase.Free;
   GGameEngine.currentMap.event[index] := nil;
   case FEvent.isTile of
      false: FBase := TCharSprite.create(FEvent, GCurrentEngine.parent, self);
      true: FBase := TEventSprite.create(FEvent, GCurrentEngine.parent, self);
   end;
   FBase.place;
   GGameEngine.currentMap.swapEvent(FBase, index);}
end;
{$WARN CONSTRUCTING_ABSTRACT ON}

procedure TRpgEvent.update;
var
   newpage, oldpage: TRpgEventPage;
begin
   if FEvent = nil then
      Exit;
   FEvent.locked := false;
   oldpage := FEvent.currentPage;
   newpage := FEvent.newCurrentPage;
{$MESSAGE WARN 'Commented out code in live unit'}
   {if assigned(newpage) and (newpage.hasScript) and (newpage.startCondition in [parallel, automatic]) and (not FEvent.playing) then
      GCurrentEngine.executeEvent(FEvent, FBase);}

   if oldpage = newpage then
      Exit;

   if ((fevent.isTile) and (FBase is TCharSprite)) or
      ((fevent.isTile = false) and (FBase is TEventSprite)) then
      switchType
   else
      FBase.updatePage(fevent.currentPage);
end;

{ TRpgVehicle }

procedure TRpgVehicle.ChangeSprite(name: string; index: integer; oldSprite: TMapSprite);
begin
   Self.setSprite(name, index);
end;

constructor TRpgVehicle.Create(mapTree: TMapTree; which: integer);
type
   TVhSpriteClass = class of TVehicleSprite;
var
   loc: TLocation;
   newSprite: TVhSpriteClass;
begin
   inherited Create;
   FVehicleIndex := which;
   if assigned(mapTree) then
   begin
      loc := mapTree.Location[which];
      FMap := loc.map;
      FX := loc.x;
      FY := loc.y;
   end;
{$MESSAGE WARN 'Commented out code in live unit'}
{   setSprite(GDatabase.SystemData.vehicleGraphic[which], GDatabase.SystemData.vehicleIndex[which]);
   case which of
      vh_boat, vh_ship: newSprite := TGroundVehicleSprite;
      vh_airship: newSprite := TAirshipSprite;
      else raise ESpriteError.Create('Bad vehicle type');
   end;
   FGameSprite := newSprite.Create(GCurrentEngine.parent, self, nil); }
   if FMap = GSpriteEngine.mapID then
      FGameSprite.location := point(FX, FY)
   else FGameSprite.location := point(0, -1);
   FGameSprite.facing := facing_left;
   (FGameSprite as TVehicleSprite).update(FSprite, false);
end;

destructor TRpgVehicle.Destroy;
begin
   FGameSprite.Free;
   inherited;
end;

procedure TRpgVehicle.doFlash(r, g, b, power: byte; time: cardinal);
begin
   if map = GSpriteEngine.mapID then
      self.gamesprite.flash(r, g, b, power, time);
end;

function TRpgVehicle.getBase: TMapSprite;
begin
   result := FGameSprite;
end;

function TRpgVehicle.getFacing: byte;
begin
   result := 0;
   case FGameSprite.facing of
      facing_up: result := 8;
      facing_right: result := 6;
      facing_down: result := 2;
      facing_left: result := 4;
   end;
end;

function TRpgVehicle.getLocation: TSgPoint;
begin
   result := point(self.x, self.y);
end;

function TRpgVehicle.getX: word;
begin
   FX := FGameSprite.location.x;
   result := FX;
end;

function TRpgVehicle.getY: word;
begin
   FY := FGameSprite.location.Y;
   result := FY;
end;

procedure TRpgVehicle.setLocation(const Value: TSgPoint);
begin
   FX := value.x;
   FY := value.Y;
   FGameSprite.location := value;
end;

procedure TRpgVehicle.setX(const Value: word);
begin
   FX := value;
   FGameSprite.location := point(FX, FGameSprite.location.Y);
end;

procedure TRpgVehicle.setY(const Value: word);
begin
   FY := value;
   FGameSprite.location := point(FGameSprite.location.x, FY);
end;

function TRpgVehicle.inUse: boolean;
begin
   result := FCarrying <> nil;
end;

procedure TRpgVehicle.setFacing(const Value: byte);
begin
   case value of
      8: FGameSprite.facing := facing_up;
      6: FGameSprite.facing := facing_right;
      4: FGameSprite.facing := facing_left;
      2: FGameSprite.facing := facing_down;
   end;
end;

procedure TRpgVehicle.setMap(const Value: smallint);
begin
   FMap := Value;
   FGameSprite.visible := (map = GSpriteEngine.mapID);
end;

procedure TRpgVehicle.setSprite(filename: string; index: byte);
var dummy: string;
begin
{$MESSAGE WARN 'Commented out code in live unit'}
   if not (index in [0..7]) then
      Exit;
   dummy := filename;
//   findGraphic(dummy, 'charset');
   if dummy = '' then
      Exit;

   FSprite := filename;
   FSpriteIndex := index;
end;

end.
