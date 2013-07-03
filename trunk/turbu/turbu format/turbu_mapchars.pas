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
unit turbu_mapchars;

interface
uses
   sdl_sprite, sg_defs,
   rsImport,
   charset_data,
   turbu_map_sprites, turbu_map_objects, turbu_map_metadata, turbu_defs,
   dwsJSON;

type
   TRpgCharacter = class(TObject)
   private
      function getScreenX: integer; inline;
      function getScreenY: integer; inline;
      function getScreenXP: integer; inline;
      function getScreenYP: integer; inline;
   protected
      procedure doFlash(r, g, b, power: integer; time: integer); virtual; abstract;
      function getX: integer; virtual; abstract;
      function getY: integer; virtual; abstract;
      function getBase: TMapSprite; virtual; abstract;
      function getTranslucency: integer; virtual;
      procedure setTranslucency(const Value: integer); virtual;
   public
      procedure flash(r, g, b, power: integer; time: integer; wait: boolean);
      procedure Move(frequency: integer; loop, skip: boolean; const path: string);
      [NoImport]
      procedure ChangeSprite(name: string; translucent: boolean); virtual; abstract;

      property screenX: integer read getScreenX;
      property screenY: integer read getScreenY;
      property screenXP: integer read getScreenXP;
      property screenYP: integer read getScreenYP;
      [NoImport]
      property base: TMapSprite read getBase;
      property translucency: integer read getTranslucency write setTranslucency;
   end;

   TRpgEvent = class(TRpgCharacter)
   private
      FID: integer;
      FBase: TMapSprite;
      FIsChar: boolean;
      FEvent: TRpgMapObject;
      FChangeSpriteName: string;
      FChangeSpriteTranslucent: boolean;
      FChangeSprite: boolean;

      function getLocation: TSgPoint;
      procedure setLocation(const Value: TSgPoint);
      function getMap: integer;
      function getFacing: integer;
      procedure InternalChangeSprite;
      function GetTFacing: TFacing;
      procedure DeserializeBase(obj: TdwsJSONObject);
   protected
      function getX: integer; override;
      function getY: integer; override;
      function getBase: TMapSprite; override;
      procedure doFlash(r, g, b, power: integer; time: integer); override;
   public
      [NoImport]
      constructor create(base: TMapSprite);
      [NoImport]
      procedure Serialize(writer: TdwsJSONWriter);
      [NoImport]
      procedure Deserialize(obj: TdwsJSONObject);
      [NoImport]
      procedure update;
      [NoImport]
      procedure ChangeSprite(name: string; translucent: boolean); override;
      [NoImport]
      procedure switchType;

      property map: integer read getMap;
      property x: integer read getX;
      property y: integer read getY;
      property xPos: integer read getX;
      property yPos: integer read getY;
      property facingValue: integer read getFacing;
      property facing: TFacing read GetTFacing;
      property id: integer read FID;
      [NoImport]
      property base: TMapSprite read FBase;
      [NoImport]
      property mapObj: TRpgMapObject read FEvent;
      property location : TSgPoint read getLocation write setLocation;
   end;

   TRpgVehicle = class(TRpgCharacter)
   private
      FSprite: string;
      FSpriteIndex: integer;
      FMap: integer;
      FX: integer;
      FY: integer;
      FGameSprite: TMapSprite;
      FVehicleIndex: integer;
      FCarrying: TRpgCharacter;

      function getLocation: TSgPoint;
      procedure setLocation(const Value: TSgPoint);
      procedure setX(const Value: integer);
      procedure setY(const Value: integer);
      procedure setMap(const Value: integer);
      function getFacing: integer;
      procedure setFacing(const Value: integer);
   protected
      function getX: integer; override;
      function getY: integer; override;
      function getBase: TMapSprite; override;
      procedure doFlash(r, g, b, power: integer; time: integer); override;
   public
      [NoImport]
      constructor Create(mapTree: TMapTree; which: integer);
      destructor Destroy; override;
      procedure setSprite(filename: string; translucent: boolean);
      [NoImport]
      procedure ChangeSprite(name: string; translucent: boolean); override;
      procedure SetMusic(name: string; fadeIn, volume, tempo, balance: integer);
      function inUse: boolean;

      property sprite: string read FSprite;
      property spriteIndex: integer read FSpriteIndex;
      property map: integer read FMap write setMap;
      property x: integer read getX write setX;
      property y: integer read getY write setY;
      property location: TSgPoint read getLocation write setLocation;
      property facing: integer read getFacing write setFacing;
      [NoImport]
      property gamesprite: TMapSprite read FGameSprite write FGameSprite;
      property vehicleIndex: integer read FVehicleIndex;
      property carrying: TRpgCharacter read FCarrying write FCarrying;
   end;

implementation
uses
   types, Classes, SysUtils,
   commons, turbu_constants, tiles, turbu_2k_char_sprites,
   turbu_database, turbu_2k_sprite_engine, turbu_2k_environment, ArchiveUtils,
   turbu_pathing, turbu_script_engine, turbu_classes;

{ TRpgCharacter }

procedure TRpgCharacter.flash(r, g, b, power: integer; time: integer; wait: boolean);
begin
   TMonitor.Enter(self);
   try
      doFlash(r, g, b, power, time);
      if wait then
         GScriptEngine.threadSleep(time * 100, true);
   finally
      TMonitor.Exit(self);
   end;
end;

function TRpgCharacter.getScreenX: integer;
begin
   result := getX - round(GSpriteEngine.WorldX / TILE_SIZE.X);
end;

function TRpgCharacter.getScreenXP: integer;
begin
   result := self.screenX * TILE_SIZE.x;
end;

function TRpgCharacter.getScreenYP: integer;
begin
   result := self.screenY * TILE_SIZE.y;
end;

function TRpgCharacter.getTranslucency: integer;
begin
   result := base.translucency;
end;

procedure TRpgCharacter.move(frequency: integer; loop, skip: boolean; const path: string);
var
   lPath: TPath;
begin
   if not assigned(self.base) then
      Exit;

   TMonitor.Enter(self);
   try
      try
         lPath := TPath.Create(path, loop);
      except
         //TODO: Log path creation failure
         on EAssertionFailed do
            Exit;
      end;
      self.base.MoveChange(lPath, clamp(frequency, 1, 8), loop, skip);
   finally
      TMonitor.Exit(self);
   end;
end;

procedure TRpgCharacter.setTranslucency(const Value: integer);
begin
   base.translucency := Value;
end;

function TRpgCharacter.getScreenY: integer;
begin
   result := getY - round(GSpriteEngine.WorldY / TILE_SIZE.Y);
end;

{ TRpgEvent }

constructor TRpgEvent.create(base: TMapSprite);
begin
   if base = nil then
      Exit;

   FIsChar := (base is TCharSprite);
   FBase := base;
   FBase.OnChangeSprite := self.ChangeSprite;
   FEvent := FBase.event;
   if FEvent <> nil then
      FID := FBase.event.id;
end;

procedure TRpgEvent.InternalChangeSprite;
begin
   FEvent.currentPage.overrideSprite(FChangeSpriteName, FChangeSpriteTranslucent);
   switchType;
   FChangeSprite := false;
end;

procedure TRpgEvent.Serialize(writer: TdwsJSONWriter);
const TRANSLUCENCIES: array[boolean] of integer = (0, 3);
begin
   writer.BeginObject;
      if FBase.location <> FEvent.location then
      begin
         writer.WriteName('Location');
         writer.BeginArray;
            writer.WriteInteger(FBase.location.x);
            writer.WriteInteger(FBase.location.y);
         writer.EndArray;
      end;
      if assigned(FEvent.currentPage) then
      begin
         writer.WriteName('Base');
         writer.BeginObject;
            writer.WriteName('PageID'); writer.WriteInteger(FEvent.currentPage.id);
            writer.CheckWrite('SpriteName', FBase.baseTile.ImageName, FEvent.currentPage.baseFilename);
            writer.CheckWrite('Transparent', FBase.translucency, TRANSLUCENCIES[FEvent.currentPage.baseTransparent]);
            if assigned(FBase.moveOrder) then
            begin
               writer.WriteName('Path');
               FBase.moveOrder.serialize(writer);
            end;
            writer.CheckWrite('MoveFreq', FBase.moveFreq, 1);
            writer.CheckWrite('MoveRate', FBase.moveRate, 1);
         writer.EndObject;
      end;
   writer.EndObject;
end;

procedure TRpgEvent.DeserializeBase(obj: TdwsJSONObject);
var
   value: TdwsJSONValue;
begin
   value := obj.Items['SpriteName'];
   if assigned(value) then
   begin
      FBase.baseTile.ImageName := value.AsString;
      value.Free;
   end;
   value := obj.Items['Transparent'];
   if assigned(value) then
   begin
      FBase.translucency := value.AsInteger;
      value.Free;
   end;
   value := obj.Items['Path'];
   if assigned(value) then
   begin
      FBase.moveOrder := TPath.Deserialize(value as TdwsJSONObject);
      value.Free;
   end;
   value := obj.Items['MoveFreq'];
   if assigned(value) then
   begin
      FBase.moveFreq := value.AsInteger;
      value.Free;
   end;
   value := obj.Items['MoveRate'];
   if assigned(value) then
   begin
      FBase.moveRate := value.AsInteger;
      value.Free;
   end;
   obj.CheckEmpty;
end;

procedure TRpgEvent.Deserialize(obj: TdwsJSONObject);
var
   value: TdwsJSONValue;
   id: integer;
begin
   value := obj.Items['Location'];
   if assigned(value) then
   begin
      FBase.location := sgPoint(value.Elements[0].AsInteger, value.Elements[1].AsInteger);
      value.Free;
   end;
   FEvent.UpdateCurrentPage;
   if assigned(FEvent.currentPage) then
   begin
      value := obj.Items['Base'] as TdwsJSONObject;
      id := value.Items['PageID'].AsInteger;
      if id <> FEvent.currentPage.id then
         raise Exception.CreateFmt('Expected FEvent.currentPage.id of %d but got %d instead.', [id, FEvent.currentPage.id]);
      value.Items['PageID'].Free;
      DeserializeBase(TdwsJSONObject(value));
   end;
   obj.CheckEmpty;
end;

procedure TRpgEvent.ChangeSprite(name: string; translucent: boolean);
begin
   TMonitor.Enter(self);
   try
      FChangeSprite := true;
      FChangeSpriteName := name;
      FChangeSpriteTranslucent := translucent;
   finally
      TMonitor.Exit(self);
   end;
end;

procedure TRpgEvent.doFlash(r, g, b, power: integer; time: integer);
begin
   FBase.flash(r, g, b, power, time);
end;

function TRpgEvent.getBase: TMapSprite;
begin
   result := self.FBase;
end;

function TRpgEvent.GetTFacing: TFacing;
begin
   if FIsChar then
      result := (FBase as TCharSprite).facing
   else result := facing_down;
end;

function TRpgEvent.getFacing: integer;
const TABLE: array[TFacing] of integer = (8, 6, 2, 4);
begin
   if FIsChar then
      result := TABLE[(FBase as TCharSprite).facing]
   else result := 2;
end;

function TRpgEvent.getLocation: TSgPoint;
begin
   result := point(self.x, self.y);
end;

function TRpgEvent.getMap: integer;
begin
   if FBase is TVehicleSprite then
      result := TVehicleSprite(FBase).template.Map
   else result := GSpriteEngine.mapID;
end;

function TRpgEvent.getX: integer;
begin
   if FIsChar then
      result := (FBase as TCharSprite).location.X
   else result := trunc(FBase.baseTile.X) div TILE_SIZE.x;
end;

function TRpgEvent.getY: integer;
begin
   if FIsChar then
      result := (FBase as TCharSprite).location.y
   else result := trunc(FBase.baseTile.y) div TILE_SIZE.y;
end;

procedure TRpgEvent.setLocation(const Value: TSgPoint);
begin
   TMonitor.Enter(self);
   try
      FBase.leaveTile;
      FBase.location := value;
   finally
      TMonitor.Exit(self);
   end;
end;

procedure TRpgEvent.switchType;
var
   old: TMapSprite;
begin
   TMonitor.Enter(self);
   try
      old := FBase;
      if FEvent.isTile then
         FBase := TEventSprite.create(FEvent, GSpriteEngine)
      else FBase := TCharSprite.create(FEvent, GSpriteEngine);
      FBase.OnChangeSprite := self.ChangeSprite;
      FBase.CopyDrawState(old);
      GSpriteEngine.SwapMapSprite(old, FBase);
      FBase.place;
   finally
      TMonitor.Exit(self);
   end;
end;

procedure TRpgEvent.update;
begin
   if FEvent = nil then
      Exit;
   FEvent.locked := false;
   if not (FEvent.updated or FChangeSprite) then
   begin
      FBase.CheckMoveChange;
      Exit;
   end;

   TMonitor.Enter(self);
   try
      if FChangeSprite then
         InternalChangeSprite
      else if ((fevent.isTile) and (FBase is TCharSprite)) or
         ((fevent.isTile = false) and (FBase is TEventSprite)) then
         switchType
      else FBase.updatePage(fevent.currentPage);
      FBase.CheckMoveChange;
   finally
      TMonitor.Exit(self);
   end;
end;

{ TRpgVehicle }

procedure TRpgVehicle.ChangeSprite(name: string; translucent: boolean);
begin
   TMonitor.Enter(self);
   try
      Self.setSprite(name, translucent);
   finally
      TMonitor.Exit(self);
   end;
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
   setSprite(GDatabase.vehicles[which].mapSprite, self.translucency > 3);

{$MESSAGE WARN 'Commented out code in live unit'}
{   case which of
      vh_boat, vh_ship: newSprite := TGroundVehicleSprite;
      vh_airship: newSprite := TAirshipSprite;
      else raise ESpriteError.Create('Bad vehicle type');
   end;}
   FGameSprite := newSprite.Create(GSpriteEngine, self, nil);
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

procedure TRpgVehicle.doFlash(r, g, b, power: integer; time: integer);
begin
   if map = GSpriteEngine.mapID then
      self.gamesprite.flash(r, g, b, power, time);
end;

function TRpgVehicle.getBase: TMapSprite;
begin
   result := FGameSprite;
end;

function TRpgVehicle.getFacing: integer;
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

function TRpgVehicle.getX: integer;
begin
   FX := FGameSprite.location.x;
   result := FX;
end;

function TRpgVehicle.getY: integer;
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

procedure TRpgVehicle.setX(const Value: integer);
begin
   FX := value;
   FGameSprite.location := point(FX, FGameSprite.location.Y);
end;

procedure TRpgVehicle.setY(const Value: integer);
begin
   FY := value;
   FGameSprite.location := point(FGameSprite.location.x, FY);
end;

function TRpgVehicle.inUse: boolean;
begin
   result := FCarrying <> nil;
end;

procedure TRpgVehicle.setFacing(const Value: integer);
begin
   case value of
      8: FGameSprite.facing := facing_up;
      6: FGameSprite.facing := facing_right;
      4: FGameSprite.facing := facing_left;
      2: FGameSprite.facing := facing_down;
   end;
end;

procedure TRpgVehicle.setMap(const Value: integer);
begin
   FMap := Value;
   FGameSprite.visible := (map = GSpriteEngine.mapID);
end;

procedure TRpgVehicle.SetMusic(name: string; fadeIn, volume, tempo,
  balance: integer);
begin
   //TODO: Implement this
end;

procedure TRpgVehicle.setSprite(filename: string; translucent: boolean);
begin
   if not GraphicExists(filename, 'mapsprite') then
      Exit;
   FSprite := filename;
   self.translucency := 3 * ord(translucent);
end;

end.
