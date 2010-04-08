unit turbu_2k_sprite_engine;

interface
uses
   types, Generics.Collections,
   charset_data, turbu_maps, turbu_map_engine, tiles, turbu_tilesets,
   turbu_map_objects, turbu_containers, turbu_map_sprites,
   sdl_sprite, sdl_canvas, SDL_ImageManager;

type
   TTileMatrix = TMatrix<TTile>;
   TTileMatrixList = class(TObjectList<TTileMatrix>);
   TMapSpriteList = class(TRpgObjectList<TMapSprite>);

   TFacingSet = set of TFacing;

   T2kSpriteEngine = class(TSpriteEngine)
   private
      FMap: TRpgMap;
      FBgImage: TSdlImage;
      FTiles: TTileMatrixList;
      FTileset: TTileset;
      FOverlapping: TFacingSet;
      FViewport: TRect;
      FMapRect: TRect;
      FCurrentLayer: integer;
      FBlank: boolean;
      FMapObjects: TMapSpriteList;

      procedure SetViewport(const viewport: TRect);
      procedure loadTileMatrix(const value: TTileList; index: integer; const viewport: TRect);
      function CreateNewTile(value: TTileRef): TTile;
      function FullCreateNewTile(x, y, layer: integer): TTile;
      function GetMaxLayer: integer; inline;
   public
      constructor Create(map: TRpgMap; const viewport: TRect;
                         canvas: TSdlCanvas; tileset: TTileset; images: TSdlImages);
      destructor Destroy; override;

      procedure assignTile(const x, y, layer: integer; const tile: TTileRef);
      procedure updateBorders(x, y, layer: integer);
      procedure Process(Sender: TObject);
      procedure AdvanceFrame;
      function GetTile(x, y, layer: integer): TTile;
      function GetTopTile(x, y: integer): TTile;
      procedure RecreateTileMatrix;

      procedure EnsureImage(const filename: string);

      property overlapping: TFacingSet read FOverlapping;
      property viewport: TRect read FViewport write SetViewport;
      property mapRect: TRect read FMapRect;
      property tileset: TTileSet read FTileset;
      property currentLayer: integer read FCurrentLayer write FCurrentLayer;
      property maxLayer: integer read GetMaxLayer;
      property mapObj: TRpgMap read FMap;
      property blank: boolean read FBlank write FBlank;
   end;

implementation
uses
   SysUtils,
   commons, turbu_constants, archiveInterface,
   SG_defs;

{ T2kSpriteEngine }

procedure T2kSpriteEngine.AdvanceFrame;
begin
   TTile.Heartbeat;
end;

procedure T2kSpriteEngine.assignTile(const x, y, layer: integer;
  const tile: TTileRef);
var
   newTile: TTile;
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

procedure T2kSpriteEngine.updateBorders(x, y, layer: integer);
var
   tile: TBorderTile;
   neighbors: TNeighborSet;

   function normalizePoint(var x, y: integer): boolean;
   var
      newX, newY: integer;
   begin
      result := true;
      newX := safeMod(x, fTiles[layer].width);
      newY := safeMod(y, fTiles[layer].height);
      result := result and ((newX = x) or (wrHorizontal in FMap.wraparound));
      result := result and ((newY = y) or (wrVertical in FMap.wraparound));
      x := newX;
      y := newY;
   end;

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
   newTile: TTile;
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
//   GArchives[IMAGE_ARCHIVE].
   if map.bgName <> '' then
      {FBgImage := TSdlImage.Create(map.bgName, map.bgName, images)}; //needs a real filename

   //populate events
   FMapObjects := TMapSpriteList.Create;
   for mapObj in map.mapObjects do
   begin
      if mapObj.id = 0 then
         Continue;
      if mapObj.isTile then
         FMapObjects.Add(TEventSprite.Create(mapObj, self, nil))
      else
         FMapObjects.Add(TCharSprite.Create(mapObj, self, nil));
   end;
end;

destructor T2kSpriteEngine.Destroy;
begin
   FMapObjects.Free;
   FTiles.Free;
   FMap.Free;
   inherited;
end;

procedure T2kSpriteEngine.EnsureImage(const filename: string);
begin
   if (filename <> '') and (not self.Images.Contains(filename)) then
      self.Images.AddSpriteFromArchive(format('mapsprite\%s.png', [filename]), filename, SPRITE_SIZE);
end;

function T2kSpriteEngine.FullCreateNewTile(x, y, layer: integer): TTile;
var
   tile: TTileRef;
begin
   tile := FMap.getTile(x, y, layer);
   result := CreateNewTile(tile);
   if assigned(result) then
      result.place(x, y, layer, tile, FTileset);
end;

function T2kSpriteEngine.GetMaxLayer: integer;
begin
   result := FTiles.Count - 1;
end;

function T2kSpriteEngine.GetTile(x, y, layer: integer): TTile;
var
   i: integer;
begin
   if not assigned(FTiles.First[x, y]) then
      for I := 0 to FTiles.Count - 1 do
         FTiles[i][x, y] := FullCreateNewTile(x, y, i);
   result := FTiles[layer][x, y];
end;

function T2kSpriteEngine.GetTopTile(x, y: integer): TTile;
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

function T2kSpriteEngine.CreateNewTile(value: TTileRef): TTile;
var
   tileType: TTileType;
   tileClass: TTileClass;
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
   result := tileClass.Create(Self, filename)
end;

{$Q+R+}
procedure T2kSpriteEngine.loadTileMatrix(const value: TTileList; index: integer; const viewport: TRect);
var
   size: TSgPoint;

   procedure EquivalizeCoords(const x, y: integer; out equivX, equivY: integer);
   var
      adjustedCoords: TSgPoint;
   begin
      adjustedCoords := (sgPoint(x, y) + size) mod size;
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
   newTile: TTile;
   tileRef: TTileRef;
   //Yay for b0rked bounds checking in packages!
   newindex: integer;
begin
   size := FMap.size;
   for y := viewport.top - 1 to viewport.top + viewport.bottom + 1 do
      for x := viewport.left - 1 to viewport.left + viewport.Right + 1 do
      begin
         EquivalizeCoords(x, y, equivX, equivY);
         if assigned(FTiles[index][equivX, equivY]) then
            Continue;

         //FIXME: Fix this when bounds checking gets fixed
         newIndex := getIndex(equivX, equivY);
         if (newIndex > high(value)) or (newIndex < low(value)) then
            raise ERangeError.Create('Tile list bounds out of range');
         tileRef := value[newIndex];
{         tileRef := value[getIndex(equivX, equivY)];}
         newTile := CreateNewTile(tileRef);
         FTiles[index][equivX, equivY] := newTile;
         if assigned(newTile) then
            newTile.place(equivX, equivY, index, tileRef, FTileset);
      end;
end;

procedure T2kSpriteEngine.Process(Sender: TObject);
begin
   self.Dead;
//   FMap.PlaceAllChars;
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
end;

end.
