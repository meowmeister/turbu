unit map_unit;
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
   classes,
   LMU, tiles, addition_sprite, move_data, chipset_data, charset_data, commons,
   {AsphyreSprite} SDL_sprite;

type
   TTileInfo = record
      tileID: integer;
      canPass: array[TFacing] of boolean;
      square: boolean;
   end;
   PTileInfo = ^TTileInfo;

   TMapGrid = array of array of array of TTileInfo;

   TRpgMap = class(TObject)
   private
      FID: integer;
      FEngine: TSpriteEngine;
      FTemplate: TMapUnit;
      FTiles: TMapGrid;
      FBgImage: TBackgroundSprite;
      FHeight: integer;
      FWidth: integer;
      FEvents: array of TAdditionSprite;
      FBlockers: array of TAdditionSprite;
      FRoutes: TRouteSet;
      FSpriteList: TSpriteList;

      function getEvent(x: word): TAdditionSprite;
      procedure setEvent(x: word; data: TAdditionSprite);
      function doesUsePano: boolean; inline;
      function getCharacters: word; inline;
      function getTile (layer: whichlayer; x,y: integer): TTile;
      procedure writeTile (layer: whichlayer; x,y: integer; const theTile: TTile);
   public
      constructor Create(theMap: TMapUnit; parent: TSpriteEngine);
      destructor Destroy; override;
      procedure deleteEvent(value: TAdditionSprite);
      procedure addRoute; inline;
      procedure placeEvents;
      procedure scanSquare;
      procedure setBG; overload;
      procedure setBG(name: string; x, y: shortint; autoX, autoY: boolean); overload;
      procedure clearBG;
      procedure drawBG;
      procedure placeChars;
      procedure clearOut;
      function indexOf(event: TAdditionSprite): integer;
      procedure swapEvent(event: TAdditionSprite; index: word);
      function canPass(x, y: integer; direction: TFacing): boolean;
      function occupied(x, y: integer): boolean;
      function blocked(x, y: integer): TAdditionSprite;
      
      property mapID: integer read FID;
      property template: TMapUnit read FTemplate;
      property height: integer read FHeight;
      property width: integer read FWidth;
      property tiles: TMapGrid read FTiles;
      property usesPano: boolean read doesUsePano;
      property characters: word read getCharacters;
      property event[x: word]: TAdditionSprite read getEvent write setEvent;
      property tile [layer: whichlayer; x,y: integer]: TTile read getTile write writeTile; default;
      property routes: TRouteSet read FRoutes;
      property spriteList: TSpriteList read FSpriteList write FSpriteList;
      property engine: TSpriteEngine read FEngine write FEngine;
   end;

const
   LAYER_COUNT = 2;

implementation
uses
   types, sysUtils,
   locate_files, chipset_graphics, charset_graphics, events, chipset,
   script_engine,
   SDL_ImageManager;

{ TRpgMap }

procedure TRpgMap.addRoute;
begin
   setLength(FRoutes, length(FRoutes) + 1);
end;

constructor TRpgMap.Create(theMap: TMapUnit; parent: TSpriteEngine);
const
   passDown = 1;
   passLeft = 2;
   passRight = 4;
   passUp = 8;
   passSquare = $30;
var
   x, y: integer;
   counter, dummy: integer;
   decodeResult: TDecodeResult;
   currentChipset: TChipset;
   info: PTileInfo;
begin
   assert(parent is TGameMap);
   FEngine := parent;
   FTemplate := theMap;
   FBgImage := nil;
   FID := theMap.mapID;
   x := theMap.width;
   y := theMap.height;
   setLength(FTiles, LAYER_COUNT, x, y);
   FHeight := y - 1;
   FWidth := x - 1;
   counter := 0;
   currentChipset := GDatabase.getChipset(FTemplate.terrain);
   for y := 0 to FHeight do
      for x := 0 to FWidth do
      begin
         with FTiles[0, x, y] do
         begin
            tileID := theMap.lowChip[counter];
            dummy := decodePassing(theMap.lowChip[counter], currentChipset);
            begin
               canPass[facing_down] := (dummy and passDown) > 0;
               canPass[facing_up] := (dummy and passup) > 0;
               canPass[facing_left] := (dummy and passLeft) > 0;
               canPass[facing_right] := (dummy and passRight) > 0;
               square := (dummy and passSquare) > 0;
            end;
         end;

         with FTiles[1, x, y] do
         begin
            tileID := theMap.highChip[counter];
            dummy := decodePassing(theMap.lowChip[counter], currentChipset);
            begin
               canPass[facing_down] := (dummy and passDown) > 0;
               canPass[facing_up] := (dummy and passup) > 0;
               canPass[facing_left] := (dummy and passLeft) > 0;
               canPass[facing_right] := (dummy and passRight) > 0;
               square := (dummy and passSquare) > 0;
            end;
         end;
//check and decide which type of tile to make
{         case theMap.lowChip[counter] of
         0..2999:
            FTiles[0, x, y] := TWaterTile.Create(parent, theMap);
         3000..3999:
            FTiles[0, x, y] := TAnimTile.Create(parent, theMap);
         4000..4999:
            FTiles[0, x, y] := TBorderTile.Create(parent, theMap);
         else
            FTiles[0, x, y] := TLowerTile.create(parent, theMap);
         end;}
         inc(counter);
      end;
   //end
end;

procedure TRpgMap.deleteEvent(value: TAdditionSprite);
var
   i, j: word;
begin
   i := 0;
   while i < length(FEvents) do
   begin
      if FEvents[i] = value then
      begin
         for j := i to high(FEvents) - 1 do
            FEvents[j] := FEvents[j + 1];
         setLength(FEvents, length(FEvents) - 1);
      end;
      inc(i);
   end;
end;

destructor TRpgMap.Destroy;
var
   x, y: integer;
begin
   FBgImage.free;
{   for x := 0 to high(FTiles) do
      for y := 0 to high(FTiles[x]) do
         FTiles[x, y].free;
      //end
   //end}
   for x := low(FEvents) to high(FEvents) do
      FEvents[x].Free;
   finalize(FTiles);
   finalize(FEvents);
   for x := 0 to high(FRoutes) do
      FRoutes[x].free;
   finalize(FRoutes);
   inherited Destroy;
end;

function TRpgMap.doesUsePano: boolean;
begin
   result := assigned(FBgImage);
end;

procedure TRpgMap.drawBG;
begin
   with FEngine do
   begin
      FBgImage.scroll;
      FBgImage.X := WorldX;
      repeat
         FBgImage.Y := WorldY;
         repeat
            FBgImage.Draw;
            FBgImage.Y := FBgImage.Y + FBgImage.PatternHeight;
         until FBgImage.y + FBgImage.OffsetY > WorldY + Canvas.Height;
         FBgImage.X := FBgImage.X + FBgImage.PatternWidth;
      until FBgImage.X + FBgImage.OffsetX > WorldX + Canvas.Width;
   end;
end;

function TRpgMap.getCharacters: word;
begin
   result := high(FEvents);
end;

function TRpgMap.getEvent(x: word): TAdditionSprite;
begin
   result := FEvents[x];
end;

{$WARN CONSTRUCTING_ABSTRACT OFF}
procedure TRpgMap.placeEvents;
var
   x, y: integer;
begin
   setLength(FEvents, 1);
   for x := 0 to FTemplate.eventCount - 1 do
   begin
      y := length(FEvents);
      setLength(FEvents, y + 1);
      case FTemplate.events[x].isTile of
         true: FEvents[y] := TEventSprite.Create(FTemplate.events[x], FEngine, nil);
         false: FEvents[y] := TCharSprite.create(FTemplate.events[x], FEngine, nil);
      end;
      GCurrentEngine.registerEvent(FEvents[y]);
   end;
   FEvents[0] := nil;
end;
{$WARN CONSTRUCTING_ABSTRACT ON}

procedure TRpgMap.scanSquare;
var
   I, J: Integer;
   mapEngine: TGameMap;
begin
   mapEngine := FEngine as TGameMap;
   for J := 1 to height - 1 do
      for I := 1 to width - 1 do
      begin
         if FTiles[0, i, j].square then
         with FTiles[0, i, j] do begin
            canPass[facing_up] := (not FTiles[0, i, j - 1].square) and (mapEngine.passable(point(i, j - 1), facing_down)
                                  or (mapEngine.passable(point(i, j + 1), facing_up)));
            canPass[facing_down] := canPass[facing_up];
         end;
      end;
   //end FOR I

   for J := 1 to height - 1 do
      for I := 1 to width - 1 do
      begin
         if FTiles[0, i, j].square and FTiles[0, i, j].canPass[facing_up] then
         with FTiles[0, i, j] do begin
            begin
               canPass[facing_left] := (j > 0) //and
                  and ((mapEngine.passable(point(i - 1, j), facing_right))
                       or ((FTiles[0, i - 1, j].square) and (mapEngine.passable(point(i - 1, j), facing_up))));
               canPass[facing_right] := (j < width) //and
                  and ((mapEngine.passable(point(i + 1, j), facing_right))
                  or ((FTiles[0, i + 1, j].square) and (mapEngine.passable(point(i + 1, j), facing_up))));
            end;
         end;
      end;
   //end FOR I
end;

procedure TRpgMap.setEvent(x: word; data: TAdditionSprite);
begin
   FEvents[x] := data;
end;

procedure TRpgMap.swapEvent(event: TAdditionSprite; index: word);
begin
   FEvents[index] := event;
end;

procedure TRpgMap.writeTile(layer: whichlayer; x, y: integer; const theTile: TTile);
begin
   self[layer,x,y].Assign(theTile);
end;

procedure TRpgMap.setBG;
begin
   FBgImage := TBackgroundSprite.Create(FEngine, FTemplate);
   FBgImage.ImageName := 'Background ' + FTemplate.panoName;
   FBgImage.Engine := GGameEngine;
end;

procedure TRpgMap.setBG(name: string; x, y: shortint; autoX, autoY: boolean);
var filename: string;
begin
   if assigned(FBgImage) and (FBgImage.ImageName <> 'Background ' + name) then
      freeAndNil(FBgImage);
   if name = '' then
      Exit;

   filename := name;
   findGraphic(filename, 'panorama');
   if filename = '' then
      raise EParseMessage.create('Background image ' + name + ' not found!');
   TGameMap(FEngine).loadBG(filename, name);
   if not assigned(FBgImage) then
      FBgImage := TBackgroundSprite.Create(FEngine, x, y, autoX, autoY)
   else begin
      FBgImage.scrollData.x := x;
      FBgImage.scrollData.y := y;
      FBgImage.scrollData.autoX := autoX;
      FBgImage.scrollData.autoY := autoY;
   end;
   FBgImage.ImageName := 'Background ' + name;
end;

function TRpgMap.canPass(x, y: integer; direction: TFacing): boolean;
var
  I: Integer;
begin
   result := true;
   for I := 0 to high(FTiles) do
      result := result and FTiles[i, x, y].canPass[direction];
end;

procedure TRpgMap.clearBG;
begin
   freeAndNil(FBgImage);
end;

procedure TRpgMap.clearOut;
var
   I: Integer;
   dummy: TAdditionSprite;
begin
   for I := 1 to high(FEvents) do
   begin
      dummy := FEvents[1];
      dummy.nuke(true);
      dummy.Free;
   end;
end;

procedure TRpgMap.placeChars;
var
  I: Integer;
begin
   for I := 0 to high(FEvents) do
      if FEvents[i] <> nil then
         FEvents[i].place;
      //end if
   for I := ord(low(GVehicles)) to ord(high(GVehicles)) do
      GVehicles[i].gamesprite.place;
   //end FOR
end;

function TRpgMap.getTile(layer: whichlayer; x, y: integer): TTile;
begin
//fixme
{   if (between(x, 0, high(FTiles)) <> x) or (between(y, 0, high(FTiles[x])) <> y) then
      result := nil
   else if layer = upper then
      result := FTiles[x, y].upperTile
   else result := FTiles[x, y];}
   result := nil;
end;

function TRpgMap.indexOf(event: TAdditionSprite): integer;
var
   I: Integer;
begin
   result := -1;
   for I := 0 to high(FEvents) do
      if FEvents[i] = event then
      begin
         result := i;
         Exit;
      end;
end;

function TRpgMap.occupied(x, y: integer): boolean;
var
  I: Integer;
begin
   result := false;
   for I := 0 to high(FEvents) do
      if (FEvents[i].location.x = x) and (FEvents[i].location.y = y) then
      begin
         result := true;
         Exit;
      end;
end;

function TRpgMap.blocked(x, y: integer): TAdditionSprite;
var
  I: Integer;
begin
   result := nil;
   for I := 0 to high(FEvents) do
      if (FBlockers[i].location.x = x) and (FEvents[i].location.y = y) then
      begin
         result := FBlockers[i];
         Exit;
      end;
end;

end.
