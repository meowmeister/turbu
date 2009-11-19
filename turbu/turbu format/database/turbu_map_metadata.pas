unit turbu_map_metadata;

interface
uses
   types, classes, DB, Generics.Collections,
   turbu_defs, turbu_classes, turbu_sounds, turbu_containers, archiveInterface;

const
   HERO_START_LOCATION = 0;
   BOAT_START_LOCATION = 1;
   SHIP_START_LOCATION = 2;
   AIRSHIP_START_LOCATION = 3;

type
   TInheritedDecision = (id_yes, id_no, id_parent);

   TMapTree = class;

   TMapMetadata = class(TRpgDatafile)
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
      FCanPort: TInheritedDecision;
      FCanEscape: TInheritedDecision;
      FCanSave: TInheritedDecision;
      FInternalFilename: TFilenameData;
      FMapEngine: shortint;
      function getMapEngine: string;
      procedure setMapEngine(const Value: string);
   protected
      class function keyChar: ansiChar; override;
   public
      constructor Load(savefile: TStream);
      procedure save(savefile: TStream); override;
      procedure upload(db: TDataSet); override;
      procedure download(db: TDataset); override;
      destructor Destroy; override;

      property parent: smallint read FParent write FParent;
      property scrollPosition: TPoint read FScrollPosition write FScrollPosition;
      property treeOpen: boolean read FTreeOpen write FTreeOpen;
      property bgmState: TInheritedDecision read FBgmState write FBgmState;
      property bgmData: TRpgMusic read FBgmData write FBgmData;
      property battleBgState: TInheritedDecision read FBattleBgState write FBattleBgState;
      property canPort: TInheritedDecision read FCanPort write FCanPort;
      property canEscape: TInheritedDecision read FCanEscape write FCanEscape;
      property canSave: TInheritedDecision read FCanSave write FCanSave;
      property internalFilename: TFilenameData read FInternalFilename write FInternalFilename;
      property mapEngine: string read getMapEngine write setMapEngine;
   end;

   TLocationList = class(TList<TLocation>);

   TMapTree = class({TRpgDataList}TRpgObjectList<TMapMetadata>)
   private
      FCurrentMap: word;
      function getLookup(x: smallint): smallint;
      procedure setLookup(x: smallint; const Value: smallint);
      function getMap(value: integer): TMapMetadata;
      function getLookupCount: integer;
   protected
      //made protected to allow access to conversion routines
      FStartLocs: TLocationList;
      FTranslationTable: TList<smallint>;
      FMapEngines: TStringList;
   public
      constructor Create;
      constructor Load(savefile: TStream);
      procedure save(savefile: TStream);
      destructor Destroy; override;

      procedure AddLookup(x: smallint);

      property lookup[x: smallint]: smallint read getLookup write setLookup;
      property currentMap: word read FCurrentMap write FCurrentMap;
      property item[value: integer]: TMapMetadata read getMap;
      property lookupCount: integer read getLookupCount;
      property location: TLocationList read FStartLocs write FStartLocs;
{      procedure searchBack(var id: word);
      procedure searchForward(var id: word);}
//      function getBgm(const whichMap: word): ansiString;}
   end;

implementation

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

procedure TMapMetadata.upload(db: TDataSet);
begin
   inherited;
   assert(false);
end;

procedure TMapMetadata.download(db: TDataset);
begin
   inherited;
   assert(false);
end;

function TMapMetadata.getMapEngine: string;
begin
   result := FOwner.FMapEngines[FMapEngine];
end;

procedure TMapMetadata.setMapEngine(const Value: string);
begin
   FMapEngine := FOwner.FMapEngines.IndexOf(Value);
   assert(FMapEngine <> -1);
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
   FTranslationTable := TList<smallint>.Create;
end;

destructor TMapTree.Destroy;
begin
   FTranslationTable.Free;
   FStartLocs.Free;
   FMapEngines.Free;
   inherited Destroy;
end;

constructor TMapTree.Load(savefile: TStream);
var
   len, i: integer;
   locs: array of TLocation;
   table: array of smallint;
begin
   //if generics worked right, this top block would be unnecessary
   self.Create;
   lassert(savefile.readChar = UpCase(TMapMetadata.keyChar));
   self.Capacity := savefile.readInt + 1;
   for I := 0 to self.Capacity - 1 do
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
   setLength(table, savefile.readWord);
   FTranslationTable.Capacity := system.length(table);
   savefile.ReadBuffer(table[0], (system.length(table) * sizeof(word)));
   FTranslationTable.AddRange(table);
   lassert(savefile.readChar = 'T');
end;

{$Q-}{$R-}
procedure TMapTree.save(savefile: TStream);
var
   iterator: TMapMetadata;
   location: TLocation;
   engine: string;
   value: smallint;
begin
   //if generics worked right, this top block would be unnecessary
   savefile.writeChar(UpCase(TMapMetadata.keyChar));
   savefile.writeInt(self.count - 1{High});
   for iterator in self do
      iterator.save(savefile);
   savefile.writeChar(TMapMetadata.keyChar);
{   inherited Save(savefile);}

   savefile.writeInt(FMapEngines.Count);
   for engine in FMapEngines do
      savefile.writeString(engine);
   savefile.writeWord(FStartLocs.Count);
   for location in FStartLocs do
      savefile.writeBuffer(location, sizeof(int64));
   savefile.writeWord(FCurrentMap);
   savefile.writeWord(FTranslationTable.count);
   for value in FTranslationTable do
      savefile.writeWord(value);
   savefile.writeChar('T');
end;

{$Q+}{$R+}

procedure TMapTree.AddLookup(x: smallint);
begin
   FTranslationTable.Add(x);
end;

function TMapTree.getLookup(x: smallint): smallint;
begin
   result := FTranslationTable[x];
end;

function TMapTree.getLookupCount: integer;
begin
   result := FTranslationTable.Count;
end;

function TMapTree.getMap(value: integer): TMapMetadata;
begin
   result := self[lookup[value]];
end;

procedure TMapTree.setLookup(x: smallint; const Value: smallint);
begin
   FTranslationTable[x] := value;
end;

end.
