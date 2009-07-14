unit turbu_map_metadata;

interface
uses
   types, classes, DB, Generics.Collections,
   turbu_defs, turbu_classes, turbu_sounds, turbu_containers, archiveInterface;

type
   TInheritedDecision = (id_yes, id_no, id_parent);

   TMapMetadata = class(TRpgDatafile)
   private
      FParent: word;
      FGen: byte;
      FScrollPosition: TPoint;
      FTreeOpen: boolean;
      FBgmState: TInheritedDecision;
      FBgmData: TRpgMusic;
      FBattleBgState: TInheritedDecision;
      FCanPort: TInheritedDecision;
      FCanEscape: TInheritedDecision;
      FCanSave: TInheritedDecision;
      FInternalFilename: TFilenameData;
   protected
      class function keyChar: ansiChar; override;
   public
      constructor Load(savefile: TStream);
      procedure save(savefile: TStream); override;
      procedure upload(db: TDataSet); override;
      procedure download(db: TDataset); override;
      destructor Destroy; override;

      property parent: word read FParent write FParent;
      property generation: byte read FGen write FGen;
      property scrollPosition: TPoint read FScrollPosition write FScrollPosition;
      property treeOpen: boolean read FTreeOpen write FTreeOpen;
      property bgmState: TInheritedDecision read FBgmState write FBgmState;
      property bgmData: TRpgMusic read FBgmData write FBgmData;
      property battleBgState: TInheritedDecision read FBattleBgState write FBattleBgState;
      property canPort: TInheritedDecision read FCanPort write FCanPort;
      property canEscape: TInheritedDecision read FCanEscape write FCanEscape;
      property canSave: TInheritedDecision read FCanSave write FCanSave;
      property internalFilename: TFilenameData read FInternalFilename write FInternalFilename;
   end;

   TLocationList = class(TList<TLocation>);

   TMapTree = class({TRpgDataList}TRpgObjectList<TMapMetadata>)
   private
      FCurrentMap: word;
      function getLookup(x: smallint): smallint;
      procedure setLookup(x: smallint; const Value: smallint);
   protected
      //made protected to allow access to conversion routines
      FStartLocs: TLocationList;
      FTranslationTable, FNodeSet: array of smallint;
   public
      constructor Create;
      constructor Load(savefile: TStream);
      procedure save(savefile: TStream);
      destructor Destroy; override;

      procedure AddLookup(x: smallint);

      property lookup[x: smallint]: smallint read getLookup write setLookup;
      property currentMap: word read FCurrentMap write FCurrentMap;
{      procedure searchBack(var id: word);
      procedure searchForward(var id: word);}
//      function getBgm(const whichMap: word): ansiString;

{      property getSize: word read projectLen;
      property vhStartMap[x: TVehicleSet]: smallint read getVehicleStartMap;
      property vhStartX[x: TVehicleSet]: word read getVehicleStartX;
      property vhStartY[x: TVehicleSet]: word read getVehicleStartY;
      property heroStartMap: smallint read FHeroStartMap;
      property heroStartX: word read FHeroStartX;
      property heroStartY: word read FHeroStartY;}
   end;

implementation

constructor TMapMetadata.Load(savefile: TStream);
begin
   inherited Load(savefile);
   FParent := savefile.ReadWord;
   FGen := savefile.readByte;
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
   readEnd(savefile);
end;

procedure TMapMetadata.save(savefile: TStream);
begin
   inherited save(savefile);
   savefile.writeWord(FParent);
   savefile.writeByte(FGen);
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

class function TMapMetadata.keyChar: ansiChar;
begin
   result := 'd';
end;

{ TMapTree }

constructor TMapTree.Create;
begin
   inherited Create;

   FStartLocs := TLocationList.Create;
end;

destructor TMapTree.Destroy;
begin
   FStartLocs.Free;
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
   self.Capacity := savefile.readInt + 1;
   for I := 0 to self.Capacity - 1 do
      self.Add(TMapMetadata.load(savefile));
   lassert(savefile.readChar = TMapMetadata.keyChar);
{   inherited Load(savefile);}

   len := savefile.readWord;
   setLength(locs, len);
   if len > 0 then
   begin
      savefile.ReadBuffer(locs[0], len * sizeof(int64));
      FStartLocs.AddRange(locs);
   end;
   FCurrentMap := savefile.readWord;
   setLength(FNodeSet, savefile.readWord);
   savefile.ReadBuffer(FNodeSet[0], (system.length(FNodeSet) * sizeof(word)));
   setLength(FTranslationTable, savefile.readWord);
   savefile.ReadBuffer(FTranslationTable[0], (system.length(FTranslationTable) * sizeof(word)));
   lassert(savefile.readChar = 'T');
end;

procedure TMapTree.save(savefile: TStream);
var
   iterator: TMapMetadata;
   location: TLocation;
begin
   //if generics worked right, this top block would be unnecessary
   savefile.writeChar(UpCase(TMapMetadata.keyChar));
   savefile.writeInt(self.count - 1{High});
   for iterator in self do
      iterator.save(savefile);
   savefile.writeChar(TMapMetadata.keyChar);
{   inherited Save(savefile);}

   savefile.writeWord(FStartLocs.Count);
   for location in FStartLocs do
      savefile.writeBuffer(location, sizeof(int64));
   savefile.writeWord(FCurrentMap);
   savefile.writeWord(system.length(FNodeSet));
   savefile.writeBuffer(FNodeSet[0], (system.length(FNodeSet) * sizeof(word)));
   savefile.writeWord(system.length(FTranslationTable));
   savefile.writeBuffer(FTranslationTable[0], (system.length(FTranslationTable) * sizeof(word)));
   savefile.writeChar('T');
end;

procedure TMapTree.AddLookup(x: smallint);
begin
   SetLength(FTranslationTable, system.length(FTranslationTable) + 1);
   FTranslationTable[system.high(FTranslationTable)] := x;
end;

function TMapTree.getLookup(x: smallint): smallint;
begin
   result := FTranslationTable[x];
end;

procedure TMapTree.setLookup(x: smallint; const Value: smallint);
begin
   FTranslationTable[x] := value;
end;

end.
