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
   classes, Generics.Collections, DB,
   turbu_classes, turbu_containers, turbu_serialization,
   sg_defs, sdl_sprite;

type
   TTileType = set of (tsBordered, tsAnimated);
   TLayerSet = set of 0..7;
   TTileAttribute = (taUp, taDown, taLeft, taRight, taCeiling, taOverhang, taCountertop);
   TTileAttributes = set of TTileAttribute;
   TAttributeList = TList<TTileAttributes>;

   TTileRef = packed record
      case boolean of
      false: (value: word);
      true: (group: byte;
             tile: byte);
   end;

   TTileGroup = class(TRpgDatafile)
   private
      FFilename: string;
      FLinkedFileName: string;
      FOcean: boolean;
      FTileType: TTileType;
      FDimensions: TSgPoint;
   protected
      class function keyChar: ansiChar; override;
   public
      constructor Load(savefile: TStream); override;
      procedure save(savefile: TStream); override;
      procedure download(db: TDataset); override;

      property filename: string read FFilename write FFilename;
      property linkedFilename: string read FLinkedFilename write FLinkedFilename;
      property ocean: boolean read FOcean write FOcean;
      property tileType: TTileType read FTileType write FTileType;
      property dimensions: TSgPoint read FDimensions write FDimensions;
   end;

   TTileGroupRecord = class(TRpgDataFile)
   private
      FLayers: TLayerSet;
      FGroup: TTileGroup;
      FAnimDir: TAnimPlayMode;
      FAttributes: TAttributeList;
      FTerrain: TList<integer>;
   protected
      class function keyChar: ansiChar; override;
   public
      constructor Load(savefile: TStream); override;
      destructor Destroy; override;
      procedure save(savefile: TStream); override;
      procedure download(db: TDataset); override;

      property group: TTileGroup read FGroup write FGroup;
      property layers: TLayerSet read FLayers write FLayers;
      property animDir: TAnimPlayMode read FAnimDir write FAnimDir;
      property attributes: TAttributeList read FAttributes write FAttributes;
      property terrain: TList<integer> read FTerrain write FTerrain;
   end;

   TTileGroupList = class({TRpgDataList}TRpgObjectList<TTileGroupRecord>); //QC 67762

   TTileSet = class(TRpgDatafile)
   private
      [NoUpload]
      FRecords: TTileGroupList;
      FHiSpeed: boolean;
      [NoUpload]
      FGroupMap: array [0..7] of TList<byte>;
      function TileCount(value: TTileGroupRecord): byte;
   protected
      class function keyChar: ansiChar; override;
   public
      constructor Create;
      constructor Load(savefile: TStream); override;
      destructor Destroy; override;
      procedure save(savefile: TStream); override;
      procedure download(db: TDataset); override;

      function tile(index: integer; layer: byte): TTileRef;

      property Records: TTileGroupList read FRecords;
      property HiSpeed: boolean read FHiSpeed write FHiSpeed;
   end;

procedure SetDatabase(value: TRpgDatafile);
function UpperLayerFilter(value: TTileGroupRecord): boolean;

implementation
uses
   turbu_database;

var
   GDatabase: TRpgDatabase;

procedure SetDatabase(value: TRpgDatafile);
begin
   GDatabase := value as TRpgDatabase;
end;

function UpperLayerFilter(value: TTileGroupRecord): boolean;
begin
   result := value.layers - [0] <> [];
end;

{ TTileGroup }

constructor TTileGroup.Load(savefile: TStream);
begin
   inherited Load(savefile);
   FFilename := savefile.readString;
   FlinkedFilename := savefile.readString;
   FOcean := savefile.readBool;
   savefile.ReadBuffer(FTileType, SizeOf(FTileType));
   savefile.ReadBuffer(FDimensions, sizeof(FDimensions));
end;

procedure TTileGroup.save(savefile: TStream);
begin
   inherited Save(savefile);
   savefile.writeString(FFilename);
   savefile.writeString(FLinkedFilename);
   savefile.writeBool(FOcean);
   savefile.WriteBuffer(FTileType, SizeOf(FTileType));
   savefile.WriteBuffer(FDimensions, sizeof(FDimensions));
end;

procedure TTileGroup.download(db: TDataset);
begin
assert(false);
end;

class function TTileGroup.keyChar: ansiChar;
begin
   result := 'g';
end;

{ TTileGroupRecord }

constructor TTileGroupRecord.Load(savefile: TStream);
var
   i: integer;
   dummy: TTileAttributes;
begin
   inherited Load(savefile);
   savefile.readBuffer(FLayers, sizeof(FLayers));
   FGroup := GDatabase.TileGroup[savefile.readString];
   savefile.readBuffer(FAnimDir, sizeof(FAnimDir));
   FAttributes := TAttributeList.Create;

   //QC 75119
{   savefile.readList<TTileAttributes>(FAttributes);
   FTerrain := TList<integer>.Create;
   savefile.readList<integer>(FTerrain);}

   i := savefile.readInt;
   FAttributes.Capacity := i;
   for I := 0 to i - 1 do
   begin
      savefile.read(dummy, sizeof(dummy));
      FAttributes.add(dummy);
   end;

   FTerrain := TList<integer>.Create;
   i := savefile.readInt;
   FTerrain.Capacity := i;
   for I := 0 to i - 1 do
      FTerrain.add(savefile.readInt);
end;

procedure TTileGroupRecord.save(savefile: TStream);
var
   attribute: TTileAttributes;
   i: integer;
begin
   inherited Save(savefile);
   savefile.writeBuffer(FLayers, sizeof(FLayers));
   savefile.writeString(FGroup.FFilename);
   savefile.writeBuffer(FAnimDir, sizeof(FAnimDir));

   //QC 75119
{   savefile.writeList<TTileAttributes>(FAttributes);
   savefile.writeList<integer>(FTerrain);}

   savefile.writeInt(FAttributes.Count);
   for attribute in FAttributes do
      savefile.Write(attribute, sizeof(attribute));

   savefile.writeInt(FTerrain.Count);
   for i in FTerrain do
      savefile.writeInt(i);
end;

destructor TTileGroupRecord.Destroy;
begin
   FAttributes.Free;
   FTerrain.Free;
   inherited Destroy;
end;

procedure TTileGroupRecord.download(db: TDataset);
begin
assert(false);
end;

class function TTileGroupRecord.keyChar: ansiChar;
begin
   result := 'r';
end;

{ TTileSet }

constructor TTileSet.Load(savefile: TStream);
var
   i, j: integer;
   layer: byte;
begin
   inherited Load(savefile);
   for i := 0 to 7 do
      FGroupMap[i] := TList<byte>.Create;
   {records.load(savefile); //Do this once QC 67762 gets fixed}
   lassert(savefile.readChar = TTileGroupRecord.keyChar);

   FRecords := TTileGroupList.Create;
   for I := 1 to savefile.readInt do
   begin
      FRecords.Add(TTileGroupRecord.Load(savefile));
      for j := 1 to tileCount(FRecords.Last) do
      begin
         for layer in FRecords.Last.FLayers do
            FGroupMap[layer].Add(i - 1);
      end;
   end;
   lassert(savefile.readChar = UpCase(TTileGroupRecord.keyChar));

   FHiSpeed := savefile.readBool;
end;

constructor TTileSet.Create;
begin
   inherited Create;
   FRecords := TTileGroupList.Create;
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

procedure TTileSet.download(db: TDataset);
begin
assert(false);
end;

class function TTileSet.keyChar: ansiChar;
begin
   result := 't';
end;

end.
