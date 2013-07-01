{ *****************************************************************************
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
  ***************************************************************************** }

unit turbu_map_metadata;

interface
uses
   types, classes, DB, Generics.Collections, RTTI,
   turbu_defs, turbu_classes, turbu_sounds, turbu_containers, archiveInterface,
   turbu_map_interface, turbu_serialization;

const
   HERO_START_LOCATION = 0;
   BOAT_START_LOCATION = 1;
   SHIP_START_LOCATION = 2;
   AIRSHIP_START_LOCATION = 3;

type
   TMapTree = class;

   BoundsUploadAttribute = class(TDBUploadAttribute)
   protected
      procedure upload(db: TDataset; field: TRttiField; instance: TObject); override;
      procedure download(db: TDataset; field: TRttiField; instance: TObject); override;
   end;

   BattlesUploadAttribute = class(TDBUploadAttribute)
   protected
      procedure upload(db: TDataset; field: TRttiField; instance: TObject); override;
      procedure download(db: TDataset; field: TRttiField; instance: TObject); override;
   end;

   TInheritedDecision = (id_parent, id_no, id_yes);

   TMapRegion = class(TRpgDatafile)
   private
      [BoundsUpload]
      FBounds: TRect;
      FEncounterScript: string;
      function GetBattleCount: integer;
      procedure SetBattleCount(const Value: integer);
   protected
      [BattlesUpload]
      FBattles: TPWordArray;
      FEncounters: T4IntArray;
      class function keyChar: ansiChar; override;
   public
      constructor Load(savefile: TStream); override;
      procedure save(savefile: TStream); override;
      procedure download(ser: TDatasetSerializer; db: TDataset); override;

      property bounds: TRect read FBounds write FBounds;
      property battles: TPWordArray read FBattles write FBattles;
      property battleCount: integer read GetBattleCount write SetBattleCount;
      property encounterScript: string read FEncounterScript write FEncounterScript;
      property encounterParams: T4IntArray read FEncounters write FEncounters;
   end;

   TRegionList = class(TRpgDataList<TMapRegion>);

   TMapMetadata = class(TRpgDatafile, IMapMetadata)
   private
      class var
      FOwner: TMapTree;
   private
      FParent: smallint;
      FScrollPosition: TPoint;
      FTreeOpen: boolean;
      FBgmState: TInheritedDecision;
      FBgmData: TRpgMusic;
      FBattleBgState: TInheritedDecision;
      FBattleBGName: string;
      FCanPort: TInheritedDecision;
      FCanEscape: TInheritedDecision;
      FCanSave: TInheritedDecision;
      FInternalFilename: TFilenameData;
      FMapEngine: shortint;
      function getMapEngine: string;
      procedure setMapEngine(const Value: string);

      function GetParent: integer;
      function GetTreeOpen: boolean;
      procedure SetParent(const Value: smallint);
   protected
      FRegions: TRegionList;
      class function keyChar: ansiChar; override;
      class function getDatasetName: string; override;
   public
      constructor Create; override;
      constructor Load(savefile: TStream); override;
      procedure save(savefile: TStream); override;
      destructor Destroy; override;

      property parent: smallint read FParent write SetParent;
      property scrollPosition: TPoint read FScrollPosition write FScrollPosition;
      property treeOpen: boolean read FTreeOpen write FTreeOpen;
      property bgmState: TInheritedDecision read FBgmState write FBgmState;
      property bgmData: TRpgMusic read FBgmData write FBgmData;
      property battleBgState: TInheritedDecision read FBattleBgState write FBattleBgState;
      property battleBgName: string read FBattleBGName write FBattleBGName;
      property canPort: TInheritedDecision read FCanPort write FCanPort;
      property canEscape: TInheritedDecision read FCanEscape write FCanEscape;
      property canSave: TInheritedDecision read FCanSave write FCanSave;
      property internalFilename: TFilenameData read FInternalFilename write FInternalFilename;
      property mapEngine: string read getMapEngine write setMapEngine;
      property Regions: TRegionList read FRegions;
   end;

   TLocationList = class(TDictionary<integer, TLocation>);

   TMapTree = class(TObject, IMapTree)
   private type

      TMapMetadataEnumeratorI = class(TInterfacedObject, IMapMetadataEnumerator)
      private
         FInternalEnumerator: TEnumerator<TMapMetadata>;
      public
         constructor Create(const tree: TMapTree);
         destructor Destroy; override;
         function GetCurrent: IMapMetadata;
         function MoveNext: boolean;
         property Current: IMapMetadata read GetCurrent;
      end;

      TMapNode = class(THierarchyTreeNode<TMapMetadata>);
      TMapDic = class(TDictionary<smallint, TMapNode>);

   private
      FCurrentMap: word;
      FTree: TMapNode;
      FLoaded: boolean;
      function getLookup(x: smallint): TMapMetadata;
      procedure setLookup(x: smallint; const Value: TMapMetadata);
      function getMap(value: integer): TMapMetadata;
      function getLookupCount: integer;
      function GetNewID: smallint;
      procedure NotifyMoved(map: TMapMetadata);

      function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
      function _AddRef: Integer; stdcall;
      function _Release: Integer; stdcall;
   private
      function GetEnumeratorI: IMapMetadataEnumerator;
      function GetCurrentMap: integer;
      function Get(x: integer): IMapMetadata;
      function GetCount: integer;
      function AddLookup(value: TMapMetadata): TMapNode;

      function IMapTree.CurrentMap = GetCurrentMap;
      function IMapTree.Count = GetCount;
      function IMapTree.GetEnumerator = GetEnumeratorI;
   protected
      //made protected to allow access to conversion routines
      FStartLocs: TLocationList;
      FTranslationTable: TMapDic;
      FMapEngines: TStringList;
   public
      constructor Create;
      constructor Load(dm: TDataModule);
      procedure save(dm: TDataModule);
      procedure saveAll(dm: TDataModule);
      destructor Destroy; override;

      procedure Add(value: TMapMetadata);
      procedure Remove(value: TMapMetadata);
      function ContainsMap(id: integer): boolean;

      function ChildrenOf(id: smallint): TList<TMapMetadata>;
      function AddNewMetadata(parent: smallint): TMapMetadata;
      function GetEnumerator: TEnumerator<TMapMetadata>;

      property lookup[x: smallint]: TMapMetadata read getLookup write setLookup;
      property currentMap: word read FCurrentMap write FCurrentMap;
      property item[value: integer]: TMapMetadata read getMap; default;
      property lookupCount: integer read getLookupCount;
      property location: TLocationList read FStartLocs;
      property Count: integer read GetCount;
   end;

implementation
uses
   SysUtils,
   uDataSetHelper;

constructor TMapMetadata.Load(savefile: TStream);
var
   i: integer;
begin
   inherited Load(savefile);
   FParent := savefile.ReadWord;
   savefile.readBuffer(FScrollPosition, sizeof(TPoint));
   FTreeOpen := savefile.readBool;
   FBgmState := TInheritedDecision(savefile.readByte);
   lassert(savefile.readChar = 'M');
   FBgmData := TRpgMusic.Load(savefile);
   lassert(savefile.readChar = 'm');
   FBattleBgState := TInheritedDecision(savefile.readByte);
   FBattleBgName := savefile.readString;
   FCanPort := TInheritedDecision(savefile.readByte);
   FCanEscape := TInheritedDecision(savefile.readByte);
   FCanSave := TInheritedDecision(savefile.readByte);
   FInternalFilename.name := savefile.readString;
   FInternalFilename.duplicates := savefile.readInt;
   FMapEngine := savefile.readByte;
   assert(savefile.readChar = TMapRegion.keyChar);
   FRegions := TRegionList.Create;
   FRegions.Capacity := savefile.readInt;
   for i := 1 to FRegions.Capacity do
      FRegions.Add(TMapRegion.Load(savefile));
   assert(savefile.readChar = UpCase(TMapRegion.keyChar));
   readEnd(savefile);
end;

procedure TMapMetadata.save(savefile: TStream);
var
   region: TMapRegion;
begin
   inherited save(savefile);
   savefile.writeWord(FParent);
   savefile.WriteBuffer(FScrollPosition, sizeof(TPoint));
   savefile.writeBool(FTreeOpen);
   savefile.writeByte(ord(FBgmState));
   savefile.writeChar('M');
   FBgmData.save(savefile);
   savefile.writeChar('m');
   savefile.writeByte(ord(FBattleBgState));
   savefile.writeString(FBattleBgName);
   savefile.writeByte(ord(FCanPort));
   savefile.writeByte(ord(FCanEscape));
   savefile.writeByte(ord(FCanSave));
   savefile.writeString(FInternalFilename.name);
   savefile.writeInt(FInternalFilename.duplicates);
   savefile.writeByte(FMapEngine);
   savefile.WriteChar (TMapRegion.keyChar);
   savefile.WriteInt(FRegions.Count);
   for region in FRegions do
      region.save(savefile);
   savefile.WriteChar(UpCase(TMapRegion.keyChar));
   writeEnd(savefile);
end;

constructor TMapMetadata.Create;
begin
   inherited Create;
   FRegions := TRegionList.Create;
   FBgmData := TRpgMusic.Create;
end;

destructor TMapMetadata.Destroy;
begin
   FRegions.Free;
   FBgmData.Free;
   inherited;
end;

class function TMapMetadata.getDatasetName: string;
begin
   result := 'metadata';
end;

function TMapMetadata.getMapEngine: string;
begin
   result := FOwner.FMapEngines[FMapEngine];
end;

function TMapMetadata.GetParent: integer;
begin
   result := FParent;
end;

function TMapMetadata.GetTreeOpen: boolean;
begin
   result := FTreeOpen;
end;

procedure TMapMetadata.setMapEngine(const Value: string);
begin
   FMapEngine := FOwner.FMapEngines.IndexOf(Value);
   assert(FMapEngine <> -1);
end;

procedure TMapMetadata.SetParent(const Value: smallint);
begin
  FParent := Value;
  FOwner.NotifyMoved(self);
end;

class function TMapMetadata.keyChar: ansiChar;
begin
   result := 'd';
end;

{ TMapTree }

constructor TMapTree.Create;
begin
   inherited Create;

   FStartLocs := TLocationList.Create;
   TMapMetadata.FOwner := self;
   FMapEngines := TStringList.Create;
   FTranslationTable := TMapDic.Create;
end;

destructor TMapTree.Destroy;
begin
   FTranslationTable.Free;
   FStartLocs.Free;
   FMapEngines.Free;
   FTree.Free;
   inherited Destroy;
end;

constructor TMapTree.Load(dm: TDataModule);
var
   db: TDataset;
   rec: variant;
   ser: TDatasetSerializer;
   meta: TMapMetadata;
begin
   self.Create;
   db := dm.FindComponent('MapTree') as TDataset;
   db.First;
   FCurrentMap := db.FieldByName('currentMap').AsInteger;
   FMapEngines.text := db.FieldByName('mapEngines').AsString;

   ser := TDatasetSerializer.Create;
   try
      db := dm.FindComponent('Metadata') as TDataset;
      for rec in db do
      begin
         meta := TMapMetadata.Create;
         meta.download(ser, db);
         self.Add(meta);
      end;
   finally
      ser.Free;
   end;

   db := dm.FindComponent('StartLocs') as TDataset;
   for rec in db do
      FStartLocs.add(rec.id, TLocation.Create(rec.map, rec.x, rec.y));

   FLoaded := true;
end;

procedure TMapTree.save(dm: TDataModule);
var
   db: TDataset;
begin
   db := dm.FindComponent('MapTree') as TDataset;
   assert(db.RecordCount in [0, 1]);
   if db.RecordCount = 0 then
      db.Append
   else db.Edit;
   db.FieldByName('currentMap').AsInteger := FCurrentMap;
   db.FieldByName('mapEngines').AsString := FMapEngines.text;
   db.Post;
end;

procedure TMapTree.saveAll(dm: TDataModule);
var
   db: TDataset;
   ser: TDatasetSerializer;
   meta: TMapMetadata;
   pair: TPair<integer, TLocation>;
begin
   save(dm);
   ser := TDatasetSerializer.Create;
   try
      db := dm.FindComponent('metadata') as TDataset;
      for meta in self do
         meta.upload(ser, db);
   finally
      ser.Free;
   end;

   db := dm.FindComponent('StartLocs') as TDataset;
   for pair in FStartLocs do
      db.AppendRecord([pair.Key, pair.Value.map, pair.Value.x, pair.Value.y]);
end;

procedure TMapTree.NotifyMoved(map: TMapMetadata);
var
   node: TMapNode;
begin
   if not FLoaded then
      Exit;
   node := FTranslationTable[map.id];
   if node.Parent.Data.id = map.parent then
      Exit;
   node.Parent := FTranslationTable[map.parent];
end;

function TMapTree.AddNewMetadata(parent: smallint): TMapMetadata;
begin
   result := TMapMetadata.Create;
   result.Name := 'NEW MAP';
   result.Id := GetNewID;
   result.FParent := parent;
   result.FOwner := self;
   self.Add(result);
end;

procedure TMapTree.Add(value: TMapMetadata);
var
   parent: TMapNode;
begin
   if self.count = 0 then
   begin
      assert(value.id = 0);
      assert(FTree = nil);
      FTree := AddLookup(value);
   end
   else begin
      assert(FTranslationTable.ContainsKey(value.id) = false);
      parent := FTranslationTable[value.parent];
      parent.Add(AddLookup(value));
   end;
end;

procedure TMapTree.Remove(value: TMapMetadata);
var
   node: TMapNode;
   id: smallint;
begin
   id := value.id;
   node := FTranslationTable[id];
   if assigned(node.Right) and (node.Right.Count > 0) then
      raise EListError.Create('Can''t delete a map tree node with children');
   FTranslationTable.Remove(id);
   if Assigned(node.Parent) then
      node.Parent.Right.Remove(node);
end;

function TMapTree.ChildrenOf(id: smallint): TList<TMapMetadata>;
var
   node: THierarchyTreeNode<TMapMetadata>;
   list: THierarchyTreeList<TMapMetadata>;
begin
   node := FTranslationTable[id];
   result := TList<TMapMetadata>.Create;
   list := node.Right;
   if assigned(list) then
   begin
      result.Capacity := list.Count;
      for node in list do
         result.Add(node.Data);
   end;
end;

function TMapTree.ContainsMap(id: integer): boolean;
begin
   result := FTranslationTable.ContainsKey(id);
end;

function TMapTree.AddLookup(value: TMapMetadata): TMapNode;
begin
   result := TMapNode.Create(value);
   FTranslationTable.Add(value.id, result);
end;

function TMapTree.getLookup(x: smallint): TMapMetadata;
begin
   result := FTranslationTable[x].data;
end;

function TMapTree.getLookupCount: integer;
begin
   result := FTranslationTable.Count;
end;

function TMapTree.getMap(value: integer): TMapMetadata;
begin
   result := lookup[value];
end;

function TMapTree.GetNewID: smallint;
var
   found: array of boolean;
   enumerator: TMapNode;
   data: TMapMetadata;
   i: integer;
begin
   setLength(found, self.Count + 1);
   for enumerator in FTranslationTable.Values do
   begin
      data := enumerator.Data;
      if Data.id >= length(found) then
         Continue;
      found[enumerator.Data.id] := true;
   end;

   for i := 0 to system.high(found) do
      if not found[i] then
         Exit(i);
   raise Exception.Create('New ID not available'); //should not see this
end;

procedure TMapTree.setLookup(x: smallint; const Value: TMapMetadata);
begin
   FTranslationTable[x].data := value;
end;

function TMapTree.Get(x: integer): IMapMetadata;
begin
   result := self.lookup[x];
end;

function TMapTree.GetCount: integer;
begin
   result := FTranslationTable.Count;
end;

function TMapTree.GetCurrentMap: integer;
begin
   result := self.currentMap;
end;

function TMapTree.GetEnumerator: TEnumerator<TMapMetadata>;
begin
   result := FTree.GetEnumerator;
end;

function TMapTree.GetEnumeratorI: IMapMetadataEnumerator;
begin
   result := TMapMetadataEnumeratorI.Create(self);
end;

function TMapTree.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

function TMapTree._AddRef: Integer;
begin
   result := -1;
end;

function TMapTree._Release: Integer;
begin
   result := -1;
end;

{ TMapTree.TMapMetadataEnumeratorI }

constructor TMapTree.TMapMetadataEnumeratorI.Create(const tree: TMapTree);
begin
   FInternalEnumerator := tree.GetEnumerator;
end;

destructor TMapTree.TMapMetadataEnumeratorI.Destroy;
begin
   FInternalEnumerator.Free;
   inherited;
end;

function TMapTree.TMapMetadataEnumeratorI.GetCurrent: IMapMetadata;
begin
   result := FInternalEnumerator.Current;
end;

function TMapTree.TMapMetadataEnumeratorI.MoveNext: boolean;
begin
   result := FInternalEnumerator.MoveNext;
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

procedure TMapRegion.download(ser: TDatasetSerializer; db: TDataset);
begin
   assert(false);
   inherited;
end;

{ BoundsUploadAttribute }

procedure BoundsUploadAttribute.download(db: TDataset; field: TRttiField;
  instance: TObject);
var
   region: TMapRegion;
begin
   region := instance as TMapRegion;
   region.FBounds.Left := db.FieldByName('bounds_left').AsInteger;
   region.FBounds.right := db.FieldByName('bounds_right').AsInteger;
   region.FBounds.top := db.FieldByName('bounds_top').AsInteger;
   region.FBounds.bottom := db.FieldByName('bounds_bottom').AsInteger;
end;

procedure BoundsUploadAttribute.upload(db: TDataset; field: TRttiField;
  instance: TObject);
var
   region: TMapRegion;
begin
   region := instance as TMapRegion;
   db.FieldByName('bounds_left').AsInteger := region.FBounds.Left;
   db.FieldByName('bounds_right').AsInteger := region.FBounds.right;
   db.FieldByName('bounds_top').AsInteger := region.FBounds.top;
   db.FieldByName('bounds_bottom').AsInteger := region.FBounds.bottom;
end;

{ BattlesUploadAttribute }

procedure BattlesUploadAttribute.download(db: TDataset; field: TRttiField;
  instance: TObject);
var
   list: TPWordArray;
   stream: TBytesStream;
begin
   stream := TBytesStream.Create((db.FieldByName('battles') as TBlobField).AsBytes);
   try
      list := (instance as TMapRegion).FBattles;
      setLength(list, stream.Size div sizeof(word));
      if length(list) > 0 then
         stream.Read(list[0], stream.Size);
   finally
      stream.Free;
   end;
end;

procedure BattlesUploadAttribute.upload(db: TDataset; field: TRttiField;
  instance: TObject);
var
   list: TPWordArray;
   stream: TBytesStream;
begin
   stream := TBytesStream.Create;
   try
      list := (instance as TMapRegion).FBattles;
      if length(list) > 0 then
         stream.Write(list[0], length(list) * sizeof(word));
      (db.FieldByName('battles') as TBlobField).AsBytes := stream.Bytes;
   finally
      stream.Free;
   end;
end;

end.
