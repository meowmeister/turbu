unit turbu_tilesets;
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
   classes, Generics.Collections, DB, RTTI,
   turbu_classes, turbu_containers, turbu_serialization,
   sg_defs, sdl_sprite;

type
   TTileType = set of (tsBordered, tsAnimated);
   TLayerSet = set of 0..7;
   TTileAttribute = (taUp, taDown, taLeft, taRight, taCeiling, taOverhang, taCountertop);
   TTileAttributes = set of TTileAttribute;
   TAttributeList = TList<TTileAttributes>;

   LayerSetUploadAttribute = class(TDBUploadAttribute)
   public
      procedure upload(db: TDataset; field: TRttiField; instance: TObject); override;
      procedure download(db: TDataset; field: TRttiField; instance: TObject); override;
   end;

   TileTypeUploadAttribute = class(TDBUploadAttribute)
   public
      procedure upload(db: TDataset; field: TRttiField; instance: TObject); override;
      procedure download(db: TDataset; field: TRttiField; instance: TObject); override;
   end;

   TileAttributesUploadAttribute = class(TDBUploadAttribute)
   public
      procedure upload(db: TDataset; field: TRttiField; instance: TObject); override;
      procedure download(db: TDataset; field: TRttiField; instance: TObject); override;
   end;

   TileTerrainUploadAttribute = class(TDBUploadAttribute)
   public
      procedure upload(db: TDataset; field: TRttiField; instance: TObject); override;
      procedure download(db: TDataset; field: TRttiField; instance: TObject); override;
   end;

   TileGroupUploadattribute = class(TDBUploadAttribute)
   public
      procedure upload(db: TDataset; field: TRttiField; instance: TObject); override;
      procedure download(db: TDataset; field: TRttiField; instance: TObject); override;
   end;

   TTileRef = packed record
      case boolean of
      false: (value: word);
      true: (group: byte;
             tile: byte);
   end;

   TTileGroup = class(TRpgDatafile)
   private
      FLinkedFileName: string;
      FOcean: boolean;
      [TileTypeUpload]
      FTileType: TTileType;
      FDimensions: TSgPoint;
   protected
      class function keyChar: ansiChar; override;
   public
      constructor Load(savefile: TStream); override;
      procedure save(savefile: TStream); override;

      property filename: string read FName write FName;
      property linkedFilename: string read FLinkedFilename write FLinkedFilename;
      property ocean: boolean read FOcean write FOcean;
      property tileType: TTileType read FTileType write FTileType;
      property dimensions: TSgPoint read FDimensions write FDimensions;
   end;

   TTileGroupRecord = class(TRpgDataFile)
   private
      [LayerSetUpload]
      FLayers: TLayerSet;
      [TileGroupUpload]
      FGroup: TTileGroup;
      FAnimDir: TAnimPlayMode;
      [TileAttributesUpload]
      FAttributes: TAttributeList;
      [TileTerrainUpload]
      FTerrain: TList<integer>;
   protected
      class function keyChar: ansiChar; override;
   public
      constructor Create; override;
      constructor Load(savefile: TStream); override;
      destructor Destroy; override;
      procedure save(savefile: TStream); override;

      property group: TTileGroup read FGroup write FGroup;
      property layers: TLayerSet read FLayers write FLayers;
      property animDir: TAnimPlayMode read FAnimDir write FAnimDir;
      property attributes: TAttributeList read FAttributes write FAttributes;
      property terrain: TList<integer> read FTerrain write FTerrain;
   end;

   TilegroupListManagerAttribute = class (TEnumerableManagerAttribute)
   protected
      procedure Clear(const instance: TValue); override;
      procedure Add(const instance, value: TValue); override;
      function CreateNew(itemType: TRttiType): TValue; override;
   end;

   [TilegroupListManager]
   TTileGroupList = class(TRpgDataList<TTileGroupRecord>); //QC 67762
   TGroupMap = array [0..7] of TList<byte>;

   TTileSet = class(TRpgDatafile)
   private
      FRecords: TTileGroupList;
      FHiSpeed: boolean;
      [NoUpload]
      FGroupMap: TGroupMap;
      function TileCount(value: TTileGroupRecord): byte;
   protected
      procedure BuildGroupMap;
      class function keyChar: ansiChar; override;
   public
      constructor Create; override;
      constructor Load(savefile: TStream); override;
      destructor Destroy; override;
      procedure save(savefile: TStream); override;
      procedure download(ser: TDatasetSerializer; db: TDataset); override;

      function tile(index: integer; layer: byte): TTileRef;

      property Records: TTileGroupList read FRecords;
      property HiSpeed: boolean read FHiSpeed write FHiSpeed;
   end;

function UpperLayerFilter(value: TTileGroupRecord): boolean;

implementation
uses
   SysUtils,
   turbu_database;

function UpperLayerFilter(value: TTileGroupRecord): boolean;
begin
   result := value.layers - [0] <> [];
end;

{ TTileGroup }

constructor TTileGroup.Load(savefile: TStream);
begin
   inherited Load(savefile);
   FlinkedFilename := savefile.readString;
   FOcean := savefile.readBool;
   savefile.ReadBuffer(FTileType, SizeOf(FTileType));
   savefile.ReadBuffer(FDimensions, sizeof(FDimensions));
end;

procedure TTileGroup.save(savefile: TStream);
begin
   inherited Save(savefile);
   savefile.writeString(FLinkedFilename);
   savefile.writeBool(FOcean);
   savefile.WriteBuffer(FTileType, SizeOf(FTileType));
   savefile.WriteBuffer(FDimensions, sizeof(FDimensions));
end;

class function TTileGroup.keyChar: ansiChar;
begin
   result := 'g';
end;

{ TTileGroupRecord }

constructor TTileGroupRecord.Create;
begin
   inherited Create;
   FAttributes := TAttributeList.Create;
   FAttributes.Capacity := 32;
   FTerrain := TList<integer>.Create;
   FTerrain.Capacity := 32;
end;

constructor TTileGroupRecord.Load(savefile: TStream);
begin
   inherited Load(savefile);
   savefile.readBuffer(FLayers, sizeof(FLayers));
   FGroup := GDatabase.TileGroup[savefile.readString];
   savefile.readBuffer(FAnimDir, sizeof(FAnimDir));

   savefile.readList<TTileAttributes>(FAttributes);
   savefile.readList<integer>(FTerrain);
end;

procedure TTileGroupRecord.save(savefile: TStream);
begin
   inherited Save(savefile);
   savefile.writeBuffer(FLayers, sizeof(FLayers));
   savefile.writeString(FGroup.filename);
   savefile.writeBuffer(FAnimDir, sizeof(FAnimDir));

   savefile.writeList<TTileAttributes>(FAttributes);
   savefile.writeList<integer>(FTerrain);

end;

destructor TTileGroupRecord.Destroy;
begin
   FAttributes.Free;
   FTerrain.Free;
   inherited Destroy;
end;

class function TTileGroupRecord.keyChar: ansiChar;
begin
   result := 'r';
end;

{ TTileSet }

constructor TTileSet.Create;
var
   i: integer;
begin
   inherited Create;
   for i := 0 to 7 do
   begin
      FGroupMap[i] := TList<byte>.Create;
      FGroupMap[i].Capacity := 256;
   end;
   FRecords := TTileGroupList.Create;
   FRecords.Capacity := 32;
end;

destructor TTileSet.Destroy;
var
   i: integer;
begin
   FRecords.Free;
   for I := low(FGroupMap) to high(FGroupMap) do
      FGroupMap[i].Free;
   inherited Destroy;
end;

procedure TTileSet.download(ser: TDatasetSerializer; db: TDataset);
begin
  inherited download(ser, db);
  BuildGroupMap;
end;

procedure TTileSet.BuildGroupMap;
var
   i, j: integer;
   layer: byte;
begin
   for i := 1 to FRecords.count - 1 do
      for j := 1 to tileCount(FRecords[i]) do
         for layer in FRecords[i].FLayers do
            FGroupMap[layer].Add(i);
end;

constructor TTileSet.Load(savefile: TStream);
var
   i: integer;
begin
   inherited Load(savefile);
   for i := 0 to 7 do
      FGroupMap[i] := TList<byte>.Create;
   {records.load(savefile); //Do this once QC 67762 gets fixed}
   lassert(savefile.readChar = TTileGroupRecord.keyChar);

   FRecords := TTileGroupList.Create;
   for I := 1 to savefile.readInt do
      FRecords.Add(TTileGroupRecord.Load(savefile));
   BuildGroupMap;
   lassert(savefile.readChar = UpCase(TTileGroupRecord.keyChar));

   FHiSpeed := savefile.readBool;
end;

procedure TTileSet.save(savefile: TStream);
var
   enumerator: TTileGroupRecord;
begin
  inherited save(savefile);
   {records.save(savefile); //Do this once QC 67762 gets fixed}
   savefile.writeChar(TTileGroupRecord.keyChar);
   savefile.writeInt(records.Count);
   for enumerator in records do
      enumerator.save(savefile);
   savefile.writeChar(UpCase(TTileGroupRecord.keyChar));

   savefile.writeBool(FHiSpeed);
end;

function TTileSet.tile(index: integer; layer: byte): TTileRef;
begin
   Result.group := FGroupMap[layer][index];
   result.tile := 0;
   dec(index);
   while (index >= 0) and (FGroupMap[layer][index] = result.group) do
   begin
      inc(result.tile);
      dec(index);
   end;
end;

function TTileSet.TileCount(value: TTileGroupRecord): byte;
begin
   if tsBordered in value.FGroup.FTileType then
      result := 1
   else if tsAnimated in value.FGroup.FTileType then
      result := 3
   else result := 48;
   {TODO: Remove these hard-coded values}
end;

class function TTileSet.keyChar: ansiChar;
begin
   result := 't';
end;

{ LayerSetUploadAttribute }

procedure LayerSetUploadAttribute.download(db: TDataset; field: TRttiField;
  instance: TObject);
var
   value: byte;
begin
   value := db.FieldByName('layers').AsInteger;
   (instance as TTileGroupRecord).FLayers := TLayerSet(value);
end;

procedure LayerSetUploadAttribute.upload(db: TDataset; field: TRttiField;
  instance: TObject);
begin
   db.FieldByName('layers').AsInteger := byte((instance as TTileGroupRecord).FLayers);
end;

{ TileTypeUploadAttribute }

procedure TileTypeUploadAttribute.download(db: TDataset; field: TRttiField;
  instance: TObject);
var
   value: byte;
begin
   value := db.FieldByName('TileType').AsInteger;
   (instance as TTileGroup).TileType := TTileType(value);
end;

procedure TileTypeUploadAttribute.upload(db: TDataset; field: TRttiField;
  instance: TObject);
begin
   db.FieldByName('TileType').AsInteger := byte((instance as TTileGroup).TileType);
end;

{ TileAttributesUploadAttribute }

procedure TileAttributesUploadAttribute.download(db: TDataset;
  field: TRttiField; instance: TObject);
var
   list: TAttributeList;
   b: byte;
   blob: TBytes;
begin
   blob := db.FieldByName('attributes').AsBytes;
   list := (instance as TTileGroupRecord).FAttributes;
   list.Clear;
   list.Capacity := length(blob);
   for b in blob do
      list.Add(TTileAttributes(b));
end;

procedure TileAttributesUploadAttribute.upload(db: TDataset; field: TRttiField;
  instance: TObject);
var
   list: TAttributeList;
   i: integer;
   blob: TBytes;
begin
   list := (instance as TTileGroupRecord).FAttributes;
   setLength(blob, list.Count);
   for i := 0 to list.Count - 1 do
      blob[i] := byte(list[i]);
   db.FieldByName('attributes').AsBytes := blob;
end;

{ TileTerrainUploadAttribute }

procedure TileTerrainUploadAttribute.download(db: TDataset; field: TRttiField;
  instance: TObject);
var
   list: TList<integer>;
   stream: TMemoryStream;
begin
   stream := TMemoryStream.Create;
   try
      (db.FieldByName('terrain') as TBlobField).SaveToStream(stream);
      list := (instance as TTileGroupRecord).FTerrain;
      list.Clear;
      list.Capacity := stream.Size div sizeof(integer);
      stream.rewind;
      while not stream.eof do
         list.Add(stream.readInt);
   finally
      stream.Free;
   end;
end;

procedure TileTerrainUploadAttribute.upload(db: TDataset; field: TRttiField;
  instance: TObject);
var
   blob: TArray<integer>;
   item: integer;
   stream: TMemoryStream;
begin
   blob := (instance as TTileGroupRecord).FTerrain.ToArray;
   stream := TMemoryStream.Create;
   try
      for item in blob do
         stream.writeInt(item);
      stream.rewind;
      (db.FieldByName('terrain') as TBlobField).LoadFromStream(stream);
   finally
      stream.Free;
   end;
end;

{ TileGroupUploadattribute }

procedure TileGroupUploadattribute.download(db: TDataset; field: TRttiField;
  instance: TObject);
begin
   (instance as TTileGroupRecord).FGroup := GDatabase.tileGroup[db.FieldByName('tilegroup').AsString];
end;

procedure TileGroupUploadattribute.upload(db: TDataset; field: TRttiField;
  instance: TObject);
begin
   db.FieldByName('tilegroup').AsString := (instance as TTileGroupRecord).FGroup.name;
end;

{ TilegroupListManagerAttribute }

procedure TilegroupListManagerAttribute.Add(const instance, value: TValue);
begin
   instance.AsType<TTileGroupList>.Add(value.AsType<TTileGroupRecord>);
end;

procedure TilegroupListManagerAttribute.Clear(const instance: TValue);
begin
   instance.AsType<TTileGroupList>.Clear;
end;

function TilegroupListManagerAttribute.CreateNew(itemType: TRttiType): TValue;
begin
   result := TTileGroupRecord.Create;
end;

end.
