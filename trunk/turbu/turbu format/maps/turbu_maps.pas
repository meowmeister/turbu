unit turbu_maps;

interface
uses
   types, classes, generics.collections, DB, RTTI,
   turbu_defs, turbu_classes, turbu_map_interface, turbu_tilesets,
   turbu_serialization, turbu_map_metadata, turbu_containers, turbu_map_objects,
   SG_defs;

type
   TTileList = packed array of TTileRef;
   TTileMap = array of TTileList;

   TMapScrollType = (stNone, stScroll, stAutoscroll);
   TScriptFormat = (sfEvents, sfScripts, sfCompiled, sfLegacy);
   TWraparound = set of (wrHorizontal, wrVertical);

   TDirs8 = (n, ne, e, se, s, sw, w, nw);
   TFuzzy = (no, yes, either);
   TNeighbors = packed array [TDirs8] of TFuzzy;

   TMapRegion = class(TRpgDatafile)
   private
      FBounds: TRect;
      FEncounterScript: string;
      function GetBattleCount: integer;
      procedure SetBattleCount(const Value: integer);
   protected
      FBattles: TPWordArray;
      FEncounters: T4IntArray;
      class function keyChar: ansiChar; override;
   public
      constructor Load(savefile: TStream); override;
      procedure save(savefile: TStream); override;
      procedure download(db: TDataset); override;

      property bounds: TRect read FBounds write FBounds;
      property battles: TPWordArray read FBattles write FBattles;
      property battleCount: integer read GetBattleCount write SetBattleCount;
      property encounterScript: string read FEncounterScript write FEncounterScript;
      property encounterParams: T4IntArray read FEncounters write FEncounters;
   end;

   TRegionList = class(TObjectList<TMapRegion>);

   UploadSetAttribute = class(TDBUploadAttribute)
   protected
      procedure upload(db: TDataset; field: TRttiField; instance: TObject); override;
      procedure download(db: TDataset; field: TRttiField; instance: TObject); override;
   end;

   UploadTilesAttribute = class(TDBUploadAttribute)
   protected
      procedure upload(db: TDataset; field: TRttiField; instance: TObject); override;
      procedure download(db: TDataset; field: TRttiField; instance: TObject); override;
   end;

   UploadSizeAttribute = class(TDBUploadAttribute)
   protected
      procedure upload(db: TDataset; field: TRttiField; instance: TObject); override;
      procedure download(db: TDataset; field: TRttiField; instance: TObject); override;
   end;

   TMapObjectList = class(TRpgObjectList<TRpgMapObject>);

   TRpgMap = class(TRpgDatafile, IRpgMap)
   private type
      TTileBlitGrid = array of array of packed array of TTileRef;
   private
      FTileset: integer;
      [UploadSize]
      FSize: TSgPoint;
      FDepth: byte;
      [UploadSet]
      FWraparound: TWraparound;
      [UploadTiles]
      FTileMap: TTileMap;
      FHasBG: boolean;
      FBgName: string;
      FVScroll: TMapScrollType;
      FHScroll: TMapScrollType;
      FScrollSpeed: TSgPoint;
      FScriptFormat: TScriptFormat;
      FScriptFile: string;
      [UploadTiles]
      FEncounterScript: string;

      procedure SetSize(const Value: TSgPoint);
      procedure SetDepth(const Value: byte);
      procedure SetScriptFormat(const Value: TScriptFormat);
      function GetBattleCount: integer;
      procedure SetBattleCount(const Value: integer);
      function GetTileset: integer;
      procedure SetTileset(const Value: integer);

      procedure blitToGrid(var grid: TTileBlitGrid; const bounds: TRect);
      procedure blitFromGrid(const grid: TTileBlitGrid; const bounds: TRect);
      function calcGridDelta(const size: TSgPoint; position: byte): TRect;
   protected
      FEncounters: T4IntArray;
      [UploadTiles]
      FBattles: TPWordArray;
      FRegions: TRegionList;
      FMapObjects: TMapObjectList;

      class function keyChar: ansiChar; override;
   public
      constructor Create(meta: TMapMetadata); overload;
      constructor Load(savefile: TStream); override;
      procedure save(savefile: TStream); override;
      destructor Destroy; override;

      procedure assignTile(const x, y, layer: integer; const tile: TTileRef);
      function getTile(const x, y, layer: integer): TTileRef;
      procedure adjustSize(const size: TSgPoint; position: byte);
      function calcBlitBounds(const size: TSgPoint; position: byte): TRect;
      procedure removeInvalidEvents;

      property tileset: integer read FTileset write FTileset;
      property size: TSgPoint read FSize write SetSize;
      property depth: byte read FDepth write SetDepth;
      property wraparound: TWraparound read FWraparound write FWraparound;
      property tileMap: TTileMap read FTileMap write FTileMap;
      property hasBackground: boolean read FHasBG write FHasBG;
      property bgName: string read FBgName write FBgName;
      property hScroll: TMapScrollType read FHScroll write FHScroll;
      property vScroll: TMapScrollType read FVScroll write FVScroll;
      property scrollSpeed: TSgPoint read FScrollSpeed write FScrollSpeed;
      property scriptFormat: TScriptFormat read FScriptFormat write SetScriptFormat;
      property scriptFile: string read FScriptFile write FScriptFile;
      property battles: TPWordArray read FBattles write FBattles;
      property battleCount: integer read GetBattleCount write SetBattleCount;
      property encounterScript: string read FEncounterScript write FEncounterScript;
      property encounterParams: T4IntArray read FEncounters write FEncounters;
      property regions: TRegionList read FRegions;
      property mapObjects: TMapObjectList read FMapObjects;
   end;

implementation
uses
   math,
   commons, turbu_constants;

{ TRpgMap }

constructor TRpgMap.Create(meta: TMapMetadata);
const
   NEW_SIZE: TSgPoint = (X: 30; y: 25);
begin
   inherited Create;
   FName := meta.name;
   FId := meta.id;
   FSize := NEW_SIZE;
   self.SetDepth(turbu_constants.LAYERS);
   FTileset := 1;
   FRegions := TRegionList.Create;
   FModified := true;
end;

constructor TRpgMap.Load(savefile: TStream);
var
   i: integer;
   len: integer;
   size: TSgPoint;
begin
   inherited Load(savefile);
   FTileset := savefile.readInt;
   savefile.ReadBuffer(size, sizeof(TSgPoint));
   self.SetSize(size);
   self.depth := savefile.readByte;
   savefile.readBuffer(FWraparound, sizeof(FWraparound));
   for I := 0 to self.depth - 1 do
      savefile.ReadBuffer(FTileMap[i, 0], length(FTileMap[i]) * sizeof(TTileRef));
   FHasBG := savefile.readBool;
   FBgName := savefile.readString;
   savefile.ReadBuffer(FHScroll, sizeof(TMapScrollType));
   savefile.ReadBuffer(FVScroll, sizeof(TMapScrollType));
   savefile.ReadBuffer(FScrollSpeed, sizeof(TSgPoint));
   savefile.ReadBuffer(FScriptFormat, sizeof(TScriptFormat));
   scriptFile := savefile.readString;
   battleCount := savefile.ReadInt;
   if battleCount > 0 then
      savefile.ReadBuffer(FBattles[0], length(FBattles) * sizeof(word));
   FEncounterScript := savefile.readString;
   savefile.ReadBuffer(FEncounters, sizeof(T4IntArray));
   len := savefile.readInt;
   FRegions := TRegionList.Create;
   FRegions.Capacity := max(len, FRegions.Capacity);
   if FRegions.Count > 0 then
   begin
      lassert(savefile.readChar = TMapRegion.keyChar);
      for i := 0 to len - 1 do
         FRegions.Add(TMapRegion.Load(savefile));
      lassert(savefile.readChar = UpCase(TMapRegion.keyChar));
   end;
   FMapObjects := TMapObjectList.Create;
   lassert(savefile.readChar = TRpgMapObject.keyChar);
   FMapObjects.Add(TRpgMapObject.Create);
   for I := 1 to savefile.readInt do
      FMapObjects.Add(TRpgMapObject.Load(savefile));
   lassert(savefile.readChar = UpCase(TRpgMapObject.keyChar));
   self.readEnd(savefile);
end;

procedure TRpgMap.removeInvalidEvents;
begin
   //TODO: implement TRpgMap.removeInvalidEvents
   //TODO: Implement events ;)
end;

procedure TRpgMap.save(savefile: TStream);
var
   region: TMapRegion;
   tileList: TTileList;
   mapObj: TRpgMapObject;
begin
   inherited Save(savefile);
   savefile.writeInt(FTileset);
   savefile.WriteBuffer(FSize, sizeof(TSgPoint));
   savefile.writeByte(FDepth);
   savefile.writeBuffer(FWraparound, sizeof(FWraparound));
   for tileList in FTileMap do
      savefile.WriteBuffer(tileList[0], length(tileList) * sizeof(TTileRef));
   savefile.writeBool(FHasBG);
   savefile.writeString(FBgName);
   savefile.WriteBuffer(FHScroll, sizeof(TMapScrollType));
   savefile.WriteBuffer(FVScroll, sizeof(TMapScrollType));
   savefile.WriteBuffer(FScrollSpeed, sizeof(TSgPoint));
   savefile.WriteBuffer(FScriptFormat, sizeof(TScriptFormat));
   savefile.WriteString(scriptFile);
   savefile.WriteInt(battleCount);
   if battleCount > 0 then
      savefile.WriteBuffer(FBattles[0], length(FBattles) * sizeof(word));
   savefile.writeString(FEncounterScript);
   savefile.WriteBuffer(FEncounters, sizeof(T4IntArray));
   savefile.writeInt(FRegions.Count);
   if FRegions.Count > 0 then
   begin
      savefile.writeChar(TMapRegion.keyChar);
      for region in FRegions do
         region.save(savefile);
      savefile.writeChar(UpCase(TMapRegion.keyChar));
   end;
   savefile.WriteChar(TRpgMapObject.keyChar);
   savefile.writeInt(FMapObjects.Count);
   for mapObj in FMapObjects do
      mapObj.save(savefile);
   savefile.writeChar(UpCase(TRpgMapObject.keyChar));
   self.writeEnd(savefile);
end;

function TRpgMap.calcBlitBounds(const size: TSgPoint; position: byte): TRect;

   function calcBounds(first, second: integer; mode: byte): TSgPoint;
   var
      midpoint, minsize: integer;
   begin
      minsize := min(first, second);
      case mode of
         0:
         begin
            result.x := 0;
            result.y := minsize;
         end;
         1:
         begin
            if first > second then
               result := calcBounds(first, second, 0)
            else
            begin
               midpoint := second div 2;
               result.x := midpoint - (first div 2);
               result.y := result.x + first;
            end;
         end;
         2:
         begin
            result.x := max(0, second - first);
            result.y := result.x + minsize;
         end;
         else assert(false);
      end;
   end;

const
   nullTile: TTileRef = (group: 0; tile: 255);
var
   halfbounds: TSgPoint;
begin
   halfbounds := calcBounds(size.x, FSize.x, position mod 3);
   result.Left := halfBounds.x;
   result.Right := halfBounds.y;
   halfbounds := calcBounds(size.y, FSize.y, position div 3);
   result.Top := halfBounds.x;
   result.Bottom := halfBounds.y;
   assert(result.Left >= 0);
   assert(result.Top >= 0);
   assert(result.Right <= FSize.x);
   assert(result.Bottom <= FSize.y);
   assert(result.Right - result.Left = min(FSize.x, size.x));
   assert(result.Bottom - result.Top = min(FSize.y, size.y));
end;

function TRpgMap.calcGridDelta(const size: TSgPoint; position: byte): TRect;

   function calcPoints(first, second, position: integer): TSgPoint;
   var
      midpoint: integer;
   begin
      case position of
         0:
         begin
            result.X := 0;
            result.Y := first - second;
         end;
         1:
         begin
            midpoint := second div 2;
            result.X := midpoint - (first div 2);
            result.Y := midpoint + (first div 2);
         end;
         2:
         begin
            result.X := first - second;
            result.Y := result.X;
         end;
      end;
   end;

var
   point: TSgPoint;
begin
   point := calcPoints(FSize.x, size.x, position mod 3);
   result.Left := point.x;
   result.Right := point.y;
   point := calcPoints(FSize.x, size.x, position div 3);
   result.Top := point.x;
   result.Bottom := point.y
end;

procedure TRpgMap.adjustSize(const size: TSgPoint; position: byte);
var
   gridSize: TSgPoint;
   grid: TTileBlitGrid;
   list: TTileList;
   delta: TRect;
begin
   assert(position in [1..9]);
   if size = FSize then
      Exit;

   dec(position);
   gridsize := sgPoint(min(FSize.x, size.x), min(FSize.y, size.y));
   delta := calcGridDelta(size, position);
   setLength(grid, Self.FDepth, gridsize.y, gridsize.x);
   blitToGrid(grid, calcBlitBounds(size, position));
   self.SetSize(size);
   for list in FTileMap do
      system.FillChar(list[0], length(list) * sizeof(TTileRef), 0);
   blitFromGrid(grid, calcBlitBounds(gridsize, position));
   removeInvalidEvents;
end;

procedure TRpgMap.blitFromGrid(const grid: TTileBlitGrid; const bounds: TRect);
var
   i, j: integer;
   lineLength: integer;
begin
   lineLength := (bounds.Right - bounds.Left) * sizeof(TTileRef);
   for j := 0 to FDepth - 1 do
      for i := 0 to high(grid[j]) do
         system.Move(grid[j, i, 0], Self.FTileMap[j, ((i + bounds.Top) * FSize.x) + bounds.Left], lineLength);
end;

procedure TRpgMap.blitToGrid(var grid: TTileBlitGrid; const bounds: TRect);
var
   i, j: integer;
   lineLength: integer;
begin
   lineLength := (bounds.Right - bounds.Left) * sizeof(TTileRef);
   for j := 0 to FDepth - 1 do
      for i := 0 to high(grid[j]) do
         system.Move(Self.FTileMap[j, ((i + bounds.Top) * FSize.x) + bounds.Left], grid[j, i, 0], lineLength);
end;

procedure TRpgMap.assignTile(const x, y, layer: integer; const tile: TTileRef);
begin
   FTileMap[layer][(y * FSize.x) + x] := tile;
end;

function TRpgMap.getTile(const x, y, layer: integer): TTileRef;
begin
   result := FTileMap[layer][(y * FSize.x) + x];
end;

destructor TRpgMap.Destroy;
begin
   FRegions.Free;
   FMapObjects.Free;
   inherited Destroy;
end;

function TRpgMap.GetBattleCount: integer;
begin
   result := Length(FBattles);
end;

function TRpgMap.GetTileset: integer;
begin
  Result := FTileset;
end;

class function TRpgMap.keyChar: ansiChar;
begin
   result := 'm';
end;

procedure TRpgMap.SetBattleCount(const Value: integer);
begin
   setLength(FBattles, Value);
end;

procedure TRpgMap.SetDepth(const Value: byte);
begin
   FDepth := Value;
   setLength(FTileMap, value);
   setSize(FSize);
end;

procedure TRpgMap.SetScriptFormat(const Value: TScriptFormat);
begin
   FScriptFormat := Value;
   //do more here
end;

procedure TRpgMap.SetSize(const Value: TSgPoint);
var
   i: integer;
begin
   FSize := Value;
   for i := low(FTileMap) to high(FTileMap) do
      setLength(FTileMap[i], value.x * value.y);
end;

procedure TRpgMap.SetTileset(const Value: integer);
begin
  FTileset := Value;
end;

{ TMapRegion }

constructor TMapRegion.Load(savefile: TStream);
begin
   inherited Load(savefile);
   savefile.ReadBuffer(FBounds, sizeof(TRect));
   battleCount := savefile.ReadInt;
   if battleCount > 0 then
      savefile.ReadBuffer(FBattles[0], length(FBattles) * sizeof(word));
   FEncounterScript := savefile.readString;
   savefile.ReadBuffer(FEncounters, sizeof(T4IntArray));
   lassert(savefile.readChar = self.keyChar);
end;

procedure TMapRegion.save(savefile: TStream);
begin
   inherited Save(savefile);
   savefile.WriteBuffer(FBounds, sizeof(TRect));
   savefile.WriteInt(battleCount);
   if battleCount > 0 then
      savefile.WriteBuffer(FBattles[0], length(FBattles) * sizeof(word));
   savefile.writeString(FEncounterScript);
   savefile.WriteBuffer(FEncounters, sizeof(T4IntArray));
   savefile.writeChar(self.keyChar);
end;

function TMapRegion.GetBattleCount: integer;
begin
   result := length(FBattles);
end;

class function TMapRegion.keyChar: ansiChar;
begin
   result := 'r';
end;

procedure TMapRegion.SetBattleCount(const Value: integer);
begin
   setLength(FBattles, Value);
end;

procedure TMapRegion.download(db: TDataset);
begin
   inherited;
   assert(false);
end;

{ UploadSetAttribute }

procedure UploadSetAttribute.download(db: TDataset; field: TRttiField;
  instance: TObject);
var
   map: TRpgMap absolute instance;
begin
   assert(instance is TRpgMap);
   map.FWraparound := TWraparound(byte(db.FieldByName('wraparound').AsInteger));
end;

procedure UploadSetAttribute.upload(db: TDataset; field: TRttiField;
  instance: TObject);
var
   map: TRpgMap absolute instance;
begin
   assert(instance is TRpgMap);
   db.FieldByName('wraparound').AsInteger := byte(map.FWraparound);
end;

{ UploadTilesAttribute }

procedure UploadTilesAttribute.download(db: TDataset; field: TRttiField;
  instance: TObject);
begin
  inherited;
   //TODO: figure this out later
end;

procedure UploadTilesAttribute.upload(db: TDataset; field: TRttiField;
  instance: TObject);
begin
  inherited;
   //figure this out later
end;

{ UploadSizeAttribute }

procedure UploadSizeAttribute.download(db: TDataset; field: TRttiField;
  instance: TObject);
begin
   //do nothing here; this is handled externally
end;

procedure UploadSizeAttribute.upload(db: TDataset; field: TRttiField;
  instance: TObject);
var
   map: TRpgMap absolute instance;
begin
   assert(instance is TRpgMap);
   db.FieldByName('size.x').AsInteger := map.FSize.x;
   db.FieldByName('size.y').AsInteger := map.FSize.y;
end;

end.
