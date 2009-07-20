unit turbu_2k_sprite_engine;

interface
uses
   types, Generics.Collections,
   charset_data, turbu_maps, turbu_map_engine, tiles, turbu_tilesets,
   sdl_sprite, sdl_canvas, SDL_ImageManager;

type
   TTileMatrix = TMatrix<TTile>;

   TFacingSet = set of TFacing;

   T2kSpriteEngine = class(TSpriteEngine)
   private
      FMap: TRpgMap;
      FBgImage: TSdlImage;
      FTiles: TObjectList<TTileMatrix>;
      FTileset: TTileset;
      FOverlapping: TFacingSet;
      FViewport: TRect;
      FMapRect: TRect;

      procedure SetViewport(const viewport: TRect);
      procedure loadTileMatrix(value: TTileList; index: integer; const viewport: TRect);
      function CreateNewTile(value: TTileRef): TTile;
   public
      constructor Create(map: TRpgMap; const viewport: TRect;
                         canvas: TSdlCanvas; tileset: TTileset; images: TSdlImages);
      destructor Destroy; override;

      property overlapping: TFacingSet read FOverlapping;
      property viewport: TRect read FViewport write SetViewport;
      property mapRect: TRect read FMapRect;
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
   FMapRect := rect(0, 0, size.x, size.y);
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
   tileGroup: TTileGroup;
begin
   tileGroup := FTileset.Records[value.group].group;
   tileType := tileGroup.tileType;
   filename := tileGroup.filename;
   if tileType = [] then
      tileClass := TLowerTile
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

procedure T2kSpriteEngine.loadTileMatrix(value: TTileList; index: integer; const viewport: TRect);
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
begin
   size := FMap.size;
   for y := viewport.top to viewport.bottom - 1 do
      for x := viewport.left to viewport.Right - 1 do
      begin
         EquivalizeCoords(x, y, equivX, equivY);
         if assigned(FTiles[index][equivX, equivY]) then
            Continue;
         tileRef := value[getIndex(equivX, equivY)];
         //don't create anything for the "blank tile"
         if smallint(tileref.value) = -1 then
            Continue;
         newTile := CreateNewTile(tileRef);
         FTiles[index][equivX, equivY] := newTile;
         newTile.place(equivX, equivY, index, tileRef, FTileset);
      end;
end;

procedure T2kSpriteEngine.SetViewport(const viewport: TRect);
var
   i: integer;
begin
   assert(assigned(FMap));
   FViewport := viewport;
   self.WorldX := viewport.Left * TILE_SIZE.X;
   self.WorldY := viewport.Top * TILE_SIZE.Y;
   if assigned(self.spriteList) then
      self.SpriteList.Clear;
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
