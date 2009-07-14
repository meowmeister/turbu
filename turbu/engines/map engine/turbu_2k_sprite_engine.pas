unit turbu_2k_sprite_engine;

interface
uses
   types, Generics.Collections,
   turbu_maps, turbu_map_engine, tiles, turbu_tilesets,
   sdl_sprite, sdl_canvas, SDL_ImageManager;

type
   TTileMatrix = TMatrix<TTile>;

   T2kSpriteEngine = class(TSpriteEngine)
   private
      FMap: TRpgMap;
      FBgImage: TSdlImage;
      FTiles: TObjectList<TTileMatrix>;
      FTileset: TTileset;
      FOverlapping: boolean;

      procedure loadTileMatrix(value: TTileList; index: integer; const viewport: TRect);
      function CreateNewTile(value: TTileRef): TTile;
   public
      constructor Create(map: TRpgMap; const viewport: TRect;
                         canvas: TSdlCanvas; tileset: TTileset; images: TSdlImages);
      destructor Destroy; override;
      procedure SetViewport(const viewport: TRect);

      property overlapping: boolean read FOverlapping;
   end;

implementation
uses
   turbu_constants,
   SG_defs;

{ T2kSpriteEngine }

constructor T2kSpriteEngine.Create(map: TRpgMap; const viewport: TRect;
            canvas: TSdlCanvas; tileset: TTileset; images: TSdlImages);
var
   i: integer;
   size: TSgPoint;
begin
   inherited Create(nil, canvas);
   self.Images := images;
   FTiles := TObjectList<TTileMatrix>.Create;
   FTileset := tileset;
   FMap := map;
   size := FMap.size;
   for i := low(FMap.tileMap) to high(FMap.tileMap) do
      FTiles.add(TTileMatrix.Create(size));
   self.SetViewport(viewport);
   if map.bgName <> '' then
      FBgImage := TSdlImage.Create(map.bgName, map.bgName, images);
end;

destructor T2kSpriteEngine.Destroy;
begin
   FTiles.Free;
   FMap.Free;
   inherited;
end;

function T2kSpriteEngine.CreateNewTile(value: TTileRef): TTile;
var
   tileType: TTileType;
   tileClass: TTileClass;
   filename: string;
begin
   tileType := FTileset.Records[value.group].group.tileType;
   filename := FTileset.Records[value.group].group.filename;
   if tileType = [] then
      tileClass := TLowerTile
   else if tileType = [tsBordered] then
      tileClass := TBorderTile
   else if tileType = [tsAnimated] then
      tileClass := TAnimTile
   else if tileType = [tsBordered, tsAnimated] then
      tileClass := TWaterTile
   else raise ESpriteError.Create('Unknown tile type.');
   result := tileClass.Create(Self, filename)
end;

procedure T2kSpriteEngine.loadTileMatrix(value: TTileList; index: integer; const viewport: TRect);
var
   size: TSgPoint;

   function GetIndex(x, y: integer): integer;
   var
      adjustedCoords: TSgPoint;
   begin
      adjustedCoords := (sgPoint(x, y) + size) mod size;
      result := (y * size.x) + x;
   end;

var
   x, y: integer;
   newTile: TTile;
   tileRef: TTileRef;
begin
   size := FMap.size;
   for y := viewport.top to viewport.bottom - 1 do
      for x := viewport.left to viewport.Right - 1 do
      begin
         if assigned(FTiles[index][x, y]) then
            Continue;
         tileRef := value[getIndex(x, y)];
         //don't create anything for the "blank tile"
         if smallint(tileref.value) = -1 then
            Continue;
         newTile := CreateNewTile(tileRef);
         FTiles[index][x, y] := newTile;
         newTile.place(x, y, index, tileRef, FTileset);
      end;
end;

procedure T2kSpriteEngine.SetViewport(const viewport: TRect);
var
   i: integer;
begin
   assert(assigned(FMap));
   self.WorldX := viewport.Left * TILE_SIZE.X;
   self.WorldY := viewport.Top * TILE_SIZE.Y;
   for i := low(FMap.tileMap) to high(FMap.tileMap) do
      loadTileMatrix(FMap.tileMap[i], i, viewport);
   FOverlapping := (viewport.Left < 0) or (viewport.Top < 0)
      or (viewport.Right > self.Width) or (viewport.Top > self.Height);

end;

end.
