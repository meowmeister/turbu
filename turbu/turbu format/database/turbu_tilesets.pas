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
   classes, Generics.Collections, DB, contnrs,
   turbu_classes,
   sg_defs, sdl_sprite;

type
   TTileType = set of (tsBordered, tsAnimated);
   TLayerSet = set of 0..7;
   TTileAttribute = (taUp, taDown, taLeft, taRight, taCeiling, taOverhang, taCountertop);
   TTileAttributes = set of TTileAttribute;
   TAttributeList = TList<TTileAttributes>;

   TTileGroup = class(TRpgDatafile)
   private
      FFilename: string;
      FTileType: TTileType;
      FDimensions: TSgPoint;
   protected
      class function keyChar: ansiChar; override;
   public
      constructor Load(savefile: TStream);
      procedure save(savefile: TStream); override;
      procedure upload(db: TDataSet); override;
      procedure download(db: TDataset); override;

      property filename: string read FFilename write FFilename;
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
      constructor Load(savefile: TStream);
      destructor Destroy; override;
      procedure save(savefile: TStream); override;
      procedure upload(db: TDataSet); override;
      procedure download(db: TDataset); override;

      property group: TTileGroup read FGroup write FGroup;
      property layers: TLayerSet read FLayers write FLayers;
      property animDir: TAnimPlayMode read FAnimDir write FAnimDir;
      property attributes: TAttributeList read FAttributes write FAttributes;
      property terrain: TList<integer> read FTerrain write FTerrain;
   end;

   TTileGroupList = {TRpgDataList}TObjectList<TTileGroupRecord>; //QC 67762

   TTileSet = class(TRpgDatafile)
   private
      FRecords: TTileGroupList;
      FHiSpeed: boolean;
   protected
      class function keyChar: ansiChar; override;
   public
      constructor Load(savefile: TStream);
      destructor Destroy; override;
      procedure save(savefile: TStream); override;
      procedure upload(db: TDataSet); override;
      procedure download(db: TDataset); override;

      property Records: TTileGroupList read FRecords write FRecords;
      property HiSpeed: boolean read FHiSpeed write FHiSpeed;
   end;

implementation
uses
   turbu_database;

{ TTileGroup }

constructor TTileGroup.Load(savefile: TStream);
begin
   inherited Load(savefile);
   FFilename := savefile.readString;
   savefile.ReadBuffer(FTileType, SizeOf(FTileType));
   savefile.ReadBuffer(FDimensions, sizeof(FDimensions));
end;

procedure TTileGroup.save(savefile: TStream);
begin
   inherited Save(savefile);
   savefile.writeString(FFilename);
   savefile.WriteBuffer(FTileType, SizeOf(FTileType));
   savefile.WriteBuffer(FDimensions, sizeof(FDimensions));
end;

procedure TTileGroup.upload(db: TDataSet);
begin
assert(false);
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
begin
   inherited Load(savefile);
   savefile.readBuffer(FLayers, sizeof(FLayers));
   FGroup := GDatabase.TileGroup[savefile.readString];
   savefile.readBuffer(FAnimDir, sizeof(FAnimDir));
   FAttributes := TAttributeList.Create;
   savefile.readList<TTileAttributes>(FAttributes);
   FTerrain := TList<integer>.Create;
   savefile.readList<integer>(FTerrain);
end;

procedure TTileGroupRecord.save(savefile: TStream);
begin
   inherited Save(savefile);
   savefile.writeBuffer(FLayers, sizeof(FLayers));
   savefile.writeString(FGroup.FFilename);
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

procedure TTileGroupRecord.upload(db: TDataSet);
begin
assert(false);
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
   i: integer;
begin
   inherited Load(savefile);
   {records.load(savefile); //Do this once QC 67762 gets fixed}
   lassert(savefile.readChar = TTileGroupRecord.keyChar);
   FRecords := TTileGroupList.Create;
   for I := 0 to savefile.readInt do
      FRecords.Add(TTileGroupRecord.Load(savefile));
   lassert(savefile.readChar = UpCase(TTileGroupRecord.keyChar));

   FHiSpeed := savefile.readBool;
end;

destructor TTileSet.Destroy;
begin
   FRecords.Free;
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

procedure TTileSet.download(db: TDataset);
begin
assert(false);
end;

procedure TTileSet.upload(db: TDataSet);
begin
assert(false);
end;

class function TTileSet.keyChar: ansiChar;
begin
   result := 't';
end;

end.
