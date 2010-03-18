unit turbu_map_metadata;

interface
uses
   types, classes, DB, Generics.Collections,
   turbu_defs, turbu_classes, turbu_sounds, turbu_containers, archiveInterface,
   turbu_map_interface;

const
   HERO_START_LOCATION = 0;
   BOAT_START_LOCATION = 1;
   SHIP_START_LOCATION = 2;
   AIRSHIP_START_LOCATION = 3;

type
   TInheritedDecision = (id_yes, id_no, id_parent);

   TMapTree = class;

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
      class function keyChar: ansiChar; override;
      class function getDatasetName: string; override;
   public
      constructor Load(savefile: TStream);
      procedure save(savefile: TStream); override;
      procedure download(db: TDataset); override;
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
   end;

   TLocationList = class(TList<TLocation>);

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

      TMapNode = class(THeirarchyTreeNode<TMapMetadata>);
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
      constructor Load(savefile: TStream);
      procedure save(savefile: TStream);
      destructor Destroy; override;

      procedure Add(value: TMapMetadata);
      procedure Remove(value: TMapMetadata);

      function ChildrenOf(id: smallint): TList<TMapMetadata>;
      function AddNewMetadata(parent: smallint): TMapMetadata;
      function GetEnumerator: TEnumerator<TMapMetadata>;

      property lookup[x: smallint]: TMapMetadata read getLookup write setLookup;
      property currentMap: word read FCurrentMap write FCurrentMap;
      property item[value: integer]: TMapMetadata read getMap; default;
      property lookupCount: integer read getLookupCount;
      property location: TLocationList read FStartLocs write FStartLocs;
      property Count: integer read GetCount;
   end;

implementation
uses
   SysUtils,
   turbu_functional;

constructor TMapMetadata.Load(savefile: TStream);
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
   readEnd(savefile);
end;

procedure TMapMetadata.save(savefile: TStream);
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
   writeEnd(savefile);
end;

destructor TMapMetadata.Destroy;
begin
   FBgmData.Free;
   inherited;
end;

procedure TMapMetadata.download(db: TDataset);
begin
   inherited;
   assert(false);
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

constructor TMapTree.Load(savefile: TStream);
var
   len, i: integer;
   locs: array of TLocation;
begin
   //if generics worked right, this top block would be unnecessary
   self.Create;
   lassert(savefile.readChar = UpCase(TMapMetadata.keyChar));
   for I := 0 to savefile.readInt do
      self.Add(TMapMetadata.load(savefile));
   lassert(savefile.readChar = TMapMetadata.keyChar);
{   inherited Load(savefile);}

   for i := 1 to savefile.readInt do
      FMapEngines.Add(savefile.readString);
   len := savefile.readWord;
   setLength(locs, len);
   if len > 0 then
   begin
      savefile.ReadBuffer(locs[0], len * sizeof(int64));
      FStartLocs.AddRange(locs);
   end;
   FCurrentMap := savefile.readWord;
   lassert(savefile.readChar = 'T');
   FLoaded := true;
end;

{$Q-}{$R-}
procedure TMapTree.save(savefile: TStream);
var
   enumerator: TMapMetadata;
   location: TLocation;
   engine: string;
begin
   //if generics worked right, this top block would be unnecessary
   savefile.writeChar(UpCase(TMapMetadata.keyChar));
   savefile.writeInt(self.count - 1{High});
   for enumerator in self do
      enumerator.save(savefile);
   savefile.writeChar(TMapMetadata.keyChar);
{   inherited Save(savefile);}

   savefile.writeInt(FMapEngines.Count);
   for engine in FMapEngines do
      savefile.writeString(engine);
   savefile.writeWord(FStartLocs.Count);
   for location in FStartLocs do
      savefile.writeBuffer(location, sizeof(int64));
   savefile.writeWord(FCurrentMap);
   savefile.writeChar('T');
end;
{$Q+}{$R+}

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
   result.FBgmData := TRpgMusic.Create;
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
   node: THeirarchyTreeNode<TMapMetadata>;
   list: THeirarchyTreeList<TMapMetadata>;
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

end.
