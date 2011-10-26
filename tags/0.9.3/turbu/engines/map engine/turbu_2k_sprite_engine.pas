unit turbu_2k_sprite_engine;

interface
uses
   types, Generics.Collections,
   turbu_2k_sprite_list,
   turbu_maps, turbu_map_engine, turbu_2k_map_tiles, turbu_tilesets,
   turbu_map_objects, turbu_map_sprites, turbu_defs, tiles,
   sdl_sprite, sdl_canvas, SDL_ImageManager, SG_defs;

type
   TTileMatrix = TMatrix<TMapTile>;
   TTileMatrixList = class(TObjectList<TTileMatrix>);

   T2kSpriteEngine = class(TSpriteEngine)
   private
      FMap: TRpgMap;
      FBgImage: TBackgroundSprite;
      FTiles: TTileMatrixList;
      FTileset: TTileset;
      FOverlapping: TFacingSet;
      FViewport: TRect;
      FMapRect: TRect;
      FCurrentLayer: integer;
      FBlank: boolean;
      FMapObjects: TMapSpriteList;
      FSpriteLocations: TSpriteLocations;
      FCurrentParty: TCharSprite;

      procedure SetViewport(const viewport: TRect);
      procedure loadTileMatrix(const value: TTileList; const index: integer; const viewport: TRect);
      function CreateNewTile(value: TTileRef): TMapTile;
      function FullCreateNewTile(x, y, layer: integer): TMapTile;
      function GetMaxLayer: integer; inline;
      function GetDefTile(layer, x, y: integer): TMapTile; inline;
      procedure DrawBG;
      function GetMapID: integer;
      function outOfBounds(x, y: integer): boolean; inline;
      function GetHeight: integer;
      function GetWidth: integer;
   public
      constructor Create(map: TRpgMap; const viewport: TRect;
                         canvas: TSdlCanvas; tileset: TTileset; images: TSdlImages);
      destructor Destroy; override;

      procedure assignTile(const x, y, layer: integer; const tile: TTileRef);
      procedure updateBorders(x, y, layer: integer);
      procedure Process(Sender: TObject);
      procedure AdvanceFrame;
      function GetTile(x, y, layer: integer): TMapTile;
      function GetTopTile(x, y: integer): TMapTile;
      procedure RecreateTileMatrix;
      function AddMapObject(obj: TRpgMapObject): TMapSprite;
      procedure DeleteMapObject(obj: TMapSprite);
      function Passable(x, y: integer; direction: TFacing): boolean; overload;
      function Passable(x, y: integer): boolean; overload;
      function Passable(location: TSgPoint; direction: TFacing; character: TMapSprite): boolean; overload;
      function Passable(location: TSgPoint; direction: TFacing): boolean; overload;
      function edgeCheck(const x, y: integer; const direction: TFacing): boolean;
      procedure EnsureImage(const filename: string);
      procedure Draw; override;
      function canExit(const x, y: integer; direction: TFacing; character: TMapSprite): boolean;
      function SpritesAt(location: TSgPoint): TArray<TMapSprite>;
      procedure AddLocation(const position: TSgPoint; character: TMapSprite);
      procedure LeaveLocation(const position: TSgPoint; character: TMapSprite);
      function normalizePoint(var x, y: integer): boolean;
      procedure ResizeCanvas;

      property overlapping: TFacingSet read FOverlapping;
      property viewport: TRect read FViewport write SetViewport;
      property mapRect: TRect read FMapRect;
      property tileset: TTileSet read FTileset;
      property currentLayer: integer read FCurrentLayer write FCurrentLayer;
      property maxLayer: integer read GetMaxLayer;
      property mapObj: TRpgMap read FMap;
      property blank: boolean read FBlank write FBlank;
      property mapObjects: TMapSpriteList read FMapObjects;
      property CurrentParty: TCharSprite read FCurrentParty write FCurrentParty;
      property Tile[layer, x, y: integer]: TMapTile read GetDefTile; default;
      property MapID: integer read GetMapID;
      property height: integer read GetHeight;
      property width: integer read GetWidth;
   end;

var
   GSpriteEngine: T2kSpriteEngine;

implementation
uses
   SysUtils,
   Collections.Base,
   commons, turbu_constants, archiveInterface, charset_data;

{ T2kSpriteEngine }

procedure T2kSpriteEngine.AdvanceFrame;
begin
   TTile.Heartbeat;
end;

procedure T2kSpriteEngine.assignTile(const x, y, layer: integer;
  const tile: TTileRef);
var
   newTile: TMapTile;
begin
   if (x >= FMap.size.X) or (y >= FMap.size.Y) then
      Exit;
   FMap.AssignTile(x, y, layer, tile);
   if assigned(FTiles[layer][x, y]) then
      FTiles[layer][x, y].Dead;
   newTile := CreateNewTile(tile);
   FTiles[layer][x, y] := newTile;
   newTile.place(x, y, layer, tile, FTileset);
end;

function T2kSpriteEngine.canExit(const x, y: integer; direction: TFacing; character: TMapSprite): boolean;
var
   opposite: TFacing;
begin
   result := false;
   opposite := opposite_facing(direction);

//you have to be able to leave the tile you're on
   if passable(x, y, direction) then
   begin

//check to see if you're moving off the edge of the map, and
//that the tile you're moving into will let you enter from
//that direction
      if edgeCheck(x, y, direction) then
         result := passable(character.inFront, opposite, character);
      //end if
   end;
end;

function T2kSpriteEngine.normalizePoint(var x, y: integer): boolean;
var
   newX, newY: integer;
begin
   result := true;
   newX := safeMod(x, fTiles[0].width);
   newY := safeMod(y, fTiles[0].height);
   result := result and ((newX = x) or (wrHorizontal in FMap.wraparound));
   result := result and ((newY = y) or (wrVertical in FMap.wraparound));
   x := newX;
   y := newY;
end;

procedure T2kSpriteEngine.updateBorders(x, y, layer: integer);
var
   tile: TBorderTile;
   neighbors: TNeighborSet;

   procedure TestInclude(x, y: integer; neighbor: TDirs8);
   begin
      if normalizePoint(x, y) then
      begin
         if assigned(FTiles[layer][x, y]) and (not tile.sharesBorder(FTiles[layer][x, y])) then
            include(neighbors, neighbor)
      end
      else include(neighbors, neighbor);
   end;

var
   tileRef: TTileRef;
   newTile: TMapTile;
begin
   if not normalizePoint(x, y) then
      Exit;
   if not (FTiles[layer][x, y] is TBorderTile) then
      Exit;

   neighbors := [];
   tile := TBorderTile(FTiles[layer][x, y]);
   testInclude(x, y - 1, n);
   testInclude(x + 1, y - 1, ne);
   testInclude(x + 1, y, e);
   testInclude(x + 1, y + 1, se);
   testInclude(x, y + 1, s);
   testInclude(x - 1, y + 1, sw);
   testInclude(x - 1, y, w);
   testInclude(x - 1, y - 1, nw);

   tileRef := FMap.GetTile(x, y, layer);
   if byte(neighbors) <> tileRef.tile then
   begin
      tileRef.tile := byte(neighbors);
      FMap.assignTile(x, y, layer, tileRef);
      FTiles[layer][x, y].Dead;
      newTile := self.CreateNewTile(tileRef);
      FTiles[layer][x, y] := newTile;
      newTile.place(X, Y, layer, tileRef, FTileset);
   end;
end;

procedure T2kSpriteEngine.AddLocation(const position: TSgPoint; character: TMapSprite);
begin
   FSpriteLocations.Add(position, character);
end;

procedure T2kSpriteEngine.LeaveLocation(const position: TSgPoint; character: TMapSprite);
begin
   FSpriteLocations.RemovePair(position, character);
end;

function T2kSpriteEngine.AddMapObject(obj: TRpgMapObject): TMapSprite;
begin
   if obj.id = 0 then
      Exit(nil);
   if obj.isTile then
      result := TEventSprite.Create(obj, self, nil)
   else
      result := TCharSprite.Create(obj, self, nil);
   FMapObjects.Add(result);
end;

procedure T2kSpriteEngine.DeleteMapObject(obj: TMapSprite);
begin
   FSpriteLocations.RemovePair(obj.location, obj);
   FMapObjects.Remove(obj);
end;

constructor T2kSpriteEngine.Create(map: TRpgMap; const viewport: TRect;
            canvas: TSdlCanvas; tileset: TTileset; images: TSdlImages);
var
   i: integer;
   size: TSgPoint;
   mapObj: TRpgMapObject;
begin
   //initial setup
   inherited Create(nil, canvas);
   self.Images := images;
   FTiles := TTileMatrixList.Create;
   FTileset := tileset;
   FMap := map;
   size := FMap.size;
   self.VisibleWidth := canvas.Width;
   self.VisibleHeight := canvas.Height;
   FMapRect := rect(0, 0, size.x, size.y);

   //create layers
   for i := low(FMap.tileMap) to high(FMap.tileMap) do
      FTiles.add(TTileMatrix.Create(size));

   //set viewport and populate it with initial tiles
   self.SetViewport(viewport);

   //load background
   if map.bgName <> '' then
   begin
      FBgImage := TBackgroundSprite.Create(self, map);
      FBgImage.Image := images.EnsureBGImage('Backgrounds/' + map.bgName + '.png', map.bgName);
   end;

   //populate events
   FMapObjects := TMapSpriteList.Create;
   for mapObj in map.mapObjects do
      addMapObject(mapObj);
   FSpriteLocations := TSpriteLocations.Create;
   GSpriteEngine := self;
end;

destructor T2kSpriteEngine.Destroy;
begin
   FSpriteLocations.Free;
   FMapObjects.Free;
   FTiles.Free;
   FMap.Free;
   inherited;
end;

procedure T2kSpriteEngine.Draw;
begin
   if assigned(FBgImage) then
      DrawBG;
   inherited;
end;

procedure T2kSpriteEngine.DrawBG;
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

function T2kSpriteEngine.edgeCheck(const x, y: integer; const direction: TFacing): boolean;
begin
   case direction of
      facing_up: result := (y > 0) or (wrVertical in FMap.wraparound);
      facing_right: result := (x < FMap.width) or (wrHorizontal in FMap.wraparound);
      facing_down: result := (y < FMap.height) or (wrVertical in FMap.wraparound);
      facing_left: result := (x > 0) or (wrHorizontal in FMap.wraparound);
      else raise ESpriteError.CreateFmt('Bad Direction value: %d is out of bounds for TFacing', [ord(direction)]);
   end;
end;

procedure T2kSpriteEngine.EnsureImage(const filename: string);
begin
   if (filename <> '') and (not self.Images.Contains(filename)) then
      self.Images.AddSpriteFromArchive(format('mapsprite\%s.png', [filename]), filename, SPRITE_SIZE);
end;

function T2kSpriteEngine.FullCreateNewTile(x, y, layer: integer): TMapTile;
var
   tile: TTileRef;
begin
   tile := FMap.getTile(x, y, layer);
   result := CreateNewTile(tile);
   if assigned(result) then
      result.place(x, y, layer, tile, FTileset);
end;

function T2kSpriteEngine.GetDefTile(layer, x, y: integer): TMapTile;
begin
   result := GetTile(x, y, layer);
end;

function T2kSpriteEngine.GetHeight: integer;
begin
   result := MapObj.size.y;
end;

function T2kSpriteEngine.GetMapID: integer;
begin
   result := MapObj.ID;
end;

function T2kSpriteEngine.GetMaxLayer: integer;
begin
   result := FTiles.Count - 1;
end;

function T2kSpriteEngine.GetTile(x, y, layer: integer): TMapTile;
var
   i: integer;
begin
   normalizePoint(x, y);
   if outOfBounds(x, y) then
      Exit(nil);
   if not assigned(FTiles.First[x, y]) then
      for I := 0 to FTiles.Count - 1 do
         FTiles[i][x, y] := FullCreateNewTile(x, y, i);
   result := FTiles[layer][x, y];
end;

function T2kSpriteEngine.GetTopTile(x, y: integer): TMapTile;
var
   i: integer;
begin
   result := GetTile(x, y, GetMaxLayer);
   if assigned(result) then
      Exit;
   for i := self.MaxLayer - 1 downto 0 do
   begin
      result := FTiles[i][x, y];
      if assigned(result) then
         Exit;
   end;
   assert(false); //should not reach this point
end;

function T2kSpriteEngine.GetWidth: integer;
begin
   result := MapObj.size.x;
end;

function T2kSpriteEngine.CreateNewTile(value: TTileRef): TMapTile;
var
   tileType: TTileType;
   tileClass: TMapTileClass;
   filename: string;
   tileGroup: TTileGroup;
begin
   //don't create anything for the "blank tile"
   if smallint(value.value) = -1 then
      Exit(nil);

   tileGroup := FTileset.Records[value.group].group;
   tileType := tileGroup.tileType;
   filename := tileGroup.filename;
   if tileType = [] then
      tileClass := TMapTile
   else if tileType = [tsBordered] then
      tileClass := TBorderTile
   else if tileType = [tsAnimated] then
      tileClass := TAnimTile
   else if tileType = [tsBordered, tsAnimated] then
   begin
      if tileGroup.ocean then
         tileClass := TOceanTile
      else tileClass := TShoreTile;
   end
   else raise ESpriteError.Create('Unknown tile type.');
   result := tileClass.Create(Self, filename);
end;

{$Q+R+}
procedure T2kSpriteEngine.loadTileMatrix(const value: TTileList; const index: integer; const viewport: TRect);
var
   size: TSgPoint;

   procedure EquivalizeCoords(const x, y: integer; out equivX, equivY: integer);
   var
      adjustedCoords: TSgPoint;
   begin
      adjustedCoords := sgPoint(x, y);
      while (adjustedCoords.x < 0) or (adjustedCoords.y < 0) do
         adjustedCoords := adjustedCoords + size;
      adjustedCoords := adjustedCoords mod size;
      equivX := adjustedCoords.x;
      equivY := adjustedCoords.y;
   end;

   function GetIndex(x, y: integer): integer;
   begin
      result := (y * size.x) + x;
   end;

var
   x, y: integer;
   equivX, equivY: integer;
   newTile: TMapTile;
   tileRef: TTileRef;
   matrix: TTileMatrix;
begin
   matrix := FTiles[index];
   size := FMap.size;
   for y := viewport.top - 1 to viewport.top + viewport.bottom + 1 do
      for x := viewport.left - 1 to viewport.left + viewport.Right + 1 do
      begin
         EquivalizeCoords(x, y, equivX, equivY);
         if assigned(matrix[equivX, equivY]) then
            Continue;

         tileRef := value[getIndex(equivX, equivY)];
         newTile := CreateNewTile(tileRef);
         matrix[equivX, equivY] := newTile;
         if assigned(newTile) then
            newTile.place(equivX, equivY, index, tileRef, FTileset);
      end;
end;

function T2kSpriteEngine.outOfBounds(x, y: integer): boolean;
begin
   result := (x < 0) or (y < 0) or (x >= FMap.width) or (y >= FMap.height);
end;

function T2kSpriteEngine.Passable(x, y: integer; direction: TFacing): boolean;
const TRANSLATE: array[TFacing] of TTileAttribute = (taUp, taRight, taDown, taLeft);
var
   tile: TTile;
   i: integer;
begin
   result := true;
   for I := 0 to MaxLayer do
   begin
      tile := GetTile(x, y, i);
      if assigned(tile) then
         result := result and (TRANSLATE[direction] in tile.attributes);
   end;
end;

function T2kSpriteEngine.Passable(x, y: integer): boolean;
var
   dir: TFacing;
begin
   result := false;
   for dir := low(TFacing) to high(TFacing) do
      result := result or Passable(x, y, dir);
end;

function T2kSpriteEngine.Passable(location: TSgPoint; direction: TFacing; character: TMapSprite): boolean;
var
   event: TArray<TMapSprite>;
begin
   event := self.SpritesAt(location);
   result := passable(location, direction) and ((event = nil) or
     ((length(event) = 1) and (event[0] = character)));
end;

function T2kSpriteEngine.Passable(location: TSgPoint; direction: TFacing): boolean;
begin
   result := passable(location.x, location.y, direction);
end;

procedure T2kSpriteEngine.Process(Sender: TObject);
var
   sprite: TMapSprite;
begin
   self.Dead;
   for sprite in FMapObjects do
   begin
      sprite.place;
      sprite.MoveTick;
   end;
   if assigned(FCurrentParty) then
   begin
      FCurrentParty.place;
      FCurrentParty.MoveTick;
   end;
//TODO: calculate map shaking, fading, and overlay colors
end;

procedure T2kSpriteEngine.RecreateTileMatrix;
var
   i: integer;
begin
   FTiles.Free;
   FTiles := TTileMatrixList.Create;
   for i := low(FMap.tileMap) to high(FMap.tileMap) do
      FTiles.add(TTileMatrix.Create(FMap.size));
   self.SetViewport(FViewport);
end;

procedure T2kSpriteEngine.ResizeCanvas;
begin
   self.Canvas.Resize;
   self.VisibleWidth := canvas.Width;
   self.VisibleHeight := canvas.Height;
end;

procedure T2kSpriteEngine.SetViewport(const viewport: TRect);
var
   i: integer;
begin
   assert(assigned(FMap));
   FViewport := viewport;
   self.WorldX := viewport.Left * TILE_SIZE.X;
   self.WorldY := viewport.Top * TILE_SIZE.Y;
   for i := low(FMap.tileMap) to high(FMap.tileMap) do
      loadTileMatrix(FMap.tileMap[i], i, viewport);
   FOverlapping := [];
   if (viewport.Left < 0) then
      include(FOverlapping, facing_left)
   else if (viewport.Right > self.Width)  then
      include(FOverlapping, facing_right);
   if (viewport.Top < 0) then
      include(FOverlapping, facing_up)
   else if viewport.Top > self.Height then
      include(FOverlapping, facing_down);
   ResizeCanvas;
end;

function T2kSpriteEngine.SpritesAt(location: TSgPoint): TArray<TMapSprite>;
begin
   if not FSpriteLocations.ContainsKey(location) then
      result := nil
   else result := FSpriteLocations[location].ToArray;
end;

end.
