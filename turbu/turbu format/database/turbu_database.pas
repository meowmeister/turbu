unit turbu_database;
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
   classes, sysUtils, Generics.Collections, DB, SyncObjs,
   dm_database, turbu_database_interface, turbu_map_interface, EB_RpgScript,
   turbu_characters, turbu_items, turbu_skills, turbu_classes, turbu_resists,
   turbu_battle_engine, turbu_map_engine, turbu_sprites, turbu_animations,
   turbu_containers, turbu_script_interface, turbu_game_data,
   turbu_map_metadata, turbu_tilesets, turbu_maps, turbu_monsters,
   turbu_serialization;

type
   TCharClassList = TRpgDataList<TClassTemplate>;
   THeroTemplateList = TRpgDataList<THeroTemplate>;
   TBattleEngineList = TRpgObjectList<TBattleEngineData>;
   TMapEngineList = TRpgObjectList<TMapEngineData>;
   TMoveMatrixArray = array of TMoveMatrix;
   TItemList = TRpgDataList<TItemTemplate>;
   TItemMatrix = array[TItemType] of TItemList;
   TSkillList = TRpgDataList<TSkillTemplate>;
   TAnimList = TRpgDataList<TAnimTemplate>;
   TAttribList = TRpgDataList<TAttributeTemplate>;
   TConditionList = TRpgDataList<TConditionTemplate>;
   TTilesetList = TRpgDataList<TTileset>;
   TTileDictionary = TObjectDictionary<string, TTileGroup>;
   TVehicleList = TRpgDataList<TVehicleTemplate>;
   TMonsterList = TRpgDataList<TRpgMonster>;
   TMonsterPartyList = TRpgDataList<TRpgMonsterParty>;
   TBattleCharList = TRpgDataList<TBattleCharAnim>;

   TRpgDataTypes = (rd_class, rd_hero, rd_command, rd_item, rd_skill, rd_anim,
                    rd_attrib, rd_condition, rd_tileset, rd_switch, rd_int,
                    rd_float, rd_string, rd_vocab, rd_tilegroup, rd_script,
                    rd_vehicles, rd_monster, rd_mparty, rd_battleChar, rd_layout,
                    rd_legacy);
   TRpgDataTypeSet = set of TRpgDataTypes;

   TBattleCommandList = class(TRpgDataList<TBattleCommand>)
   public
      function indexOf (name: string): integer;
   end;

   TMapLoadProc = reference to procedure(tree: IMapTree);

   TLegacyData = class(TRpgDatafile)
   private
      FSection: integer;
      FData: rawByteString;
   protected
      class function keyChar: ansiChar; override;
   public
      constructor Create(const name: string; id, section: integer; const data: RawByteString); reintroduce;
      property section: integer read FSection;
      property value: RawByteString read FData;
   end;

   TLegacyDataList = TRpgObjectList<TLegacyData>;

   TRpgDatabase = class(TRpgDatafile, IRpgDatabase)
   private
      FClass: TCharClassList;
      FHero: THeroTemplateList;
      FCommand: TBattleCommandList;
      FItems: TItemMatrix;
      FSkills: TSkillList;
      FAnims: TAnimList;
      FAttributes: TAttribList;
      FConditions: TConditionList;
      FVehicles: TVehicleList;
      FMonsters: TMonsterList;
      FMonsterParties: TMonsterPartyList;
      FBattleChars: TBattleCharList;

      //I need terrains and system data
      //and a battle layout section

      FGlobalEvents: TMapObjectList;
      FLayout: TGameLayout;
      FSwitches: TStringList;
      FVariables: TStringList;
      FFloats: TStringList;
      FStrings: TStringList;

      FMapTree: TMapTree;

      FMoveMatrix: TMoveMatrixArray;
      FBattleStyle: TBattleEngineList;
      FMapEngines: TMapEngineList;
      FStatSet: TStatSet;
      FTileGroup: TTileDictionary;
      FTileset: TTilesetList;
      FUploadedTypes: TRpgDataTypeSet;
      FScriptFormat: TScriptFormat;
      FScriptFile: string;
      FLegacyData: TLegacyDataList;

      FFilename: string;
      FScriptLoaded: TSimpleEvent;
      FScriptLoadError: string;
      procedure SetScriptFormat(const Value: TScriptFormat);
      function getClassCount: integer;
      procedure setClassCount(const Value: integer);
      function getCommandCount: integer;
      procedure setCommandCount(const Value: integer);
      function getHeroCount: integer;
      procedure setHeroCount(const Value: integer);

      function sumItems: integer;
      function getProjectName: string;

      procedure uploadStringList(dataset: TDataset; list: TStringList);
      procedure UploadGlobalEvents(db: TdmDatabase);
      procedure UploadVocab(db: TdmDatabase);
      procedure UploadTileGroups(db: TdmDatabase);

      function GetMapTree: IMapTree;
      function IRpgDatabase.MapTree = GetMapTree;
   private
      FSerializer: TDatasetSerializer;
      procedure SaveScripts;
      procedure SaveToDB(db: TdmDatabase);
      procedure DownloadTileGroups(db: TdmDatabase);
   protected
      FGlobalScriptBlock: TEBUnit;
      FSysVocab: TStringList;
      FCustomVocab: TStringList;
      class function keyChar: ansiChar; override;
   public
      constructor Create; override;
      constructor Load(dm: TDmDatabase); reintroduce;
      destructor Destroy; override;
      procedure save(dm: TDmDatabase); reintroduce; overload;
      procedure save; reintroduce; overload;
      procedure copyToDB(db: TdmDatabase; typeSet: TRpgDataTypeSet = [];
        report: TUploadReportProc = nil);
      procedure copyTypeToDB(db: TdmDatabase; value: TRpgDataTypes);

      procedure addClass;

      procedure addHero;

      procedure addItem(slot: TItemType); overload;
      function findItem(id: integer; slot: TItemType): integer;
      function findItemById(id: integer): TItemTemplate;
      function countItems: cardinal;

      procedure addSkill;

      procedure addAnim;

      procedure addAttribute;
      procedure addCondition;
      procedure addTileset;

      procedure AddLegacy(const name: string; id, section: integer; const data: RawByteString);

      property charClasses: integer read getClassCount write setClassCount;
      property charClass: TCharClassList read FClass write FClass;
      property heroes: integer read getHeroCount write setHeroCount;
      property hero: THeroTemplateList read FHero write FHero;
      property skill: TSkillList read FSkills write FSkills;
      property item: TItemMatrix read FItems write FItems;
      property items: integer read sumItems;
      property anim: TAnimList read FAnims write FAnims;
      property attributes: TAttribList read FAttributes write FAttributes;
      property conditions: TConditionList read FConditions write FConditions;
      property command: TBattleCommandList read FCommand write FCommand;
      property commands: integer read getCommandCount write setCommandCount;
      property tileset: TTilesetList read FTileset write FTileset;
      property globalEvents: TMapObjectList read FGlobalEvents;
      property layout: TGameLayout read FLayout write FLayout;
      property variable: TStringList read FVariables write FVariables;
      property switch: TStringList read FSwitches write FSwitches;
      property battleStyle: TBattleEngineList read FBattleStyle;
      property mapEngines: TMapEngineList read FMapEngines;
      property moveMatrix: TMoveMatrixArray read FMoveMatrix write FMoveMatrix;
      property mapTree: TMapTree read FMapTree write FMapTree;
      property statSet: TStatSet read FStatSet write FStatSet;
      property tileGroup: TTileDictionary read FTileGroup;
      property scriptFormat: TScriptFormat read FScriptFormat write SetScriptFormat;
      property scriptFile: string read FScriptFile write FScriptFile;
      property scriptBlock: TEBUnit read FGlobalScriptBlock;
      property vehicles: TVehicleList read FVehicles;
      property monsters: TMonsterList read FMonsters;
      property monsterParties: TMonsterPartyList read FMonsterParties;
      property battleChars: TBattleCharList read FBattleChars;

      property projectName: string read getProjectName;
      property uploadedTypes: TRpgDataTypeSet read FUploadedTypes;
      property Filename: string read FFilename write FFilename;
      property serializer: TDatasetSerializer read FSerializer;
      property ScriptLoaded: TSimpleEvent read FScriptLoaded;
      property scriptLoadError: string read FScriptLoadError;
   end;

var
   GDatabase: TRpgDatabase;
   GScriptEngine: IScriptEngine;

implementation
uses
   Windows,
   zlib, math, TypInfo,
   archiveInterface, commons,
   turbu_constants, turbu_engines, turbu_versioning, turbu_plugin_interface,
   turbu_functional, turbu_map_objects,
   uDatasetHelper;

{ TLegacyData }

constructor TLegacyData.Create(const name: string; id, section: integer;
  const data: RawByteString);
begin
   inherited Create;
   FName := name;
   FID := id;
   FSection := section;
   FData:= data;
end;

const
   MIN_DBVERSION = 40;
   DBVERSION = 40;

class function TLegacyData.keyChar: ansiChar;
begin
   result := 'l';
end;

{ TRpgDatabase }

constructor TRpgDatabase.Create;
var
   i: TItemType;
begin
   inherited Create;
   GDatabase := self;
   FName := 'TURBU RPG Database';
   FID := DBVERSION;
   for I := low(TItemType) to high(TItemType) do
      FItems[i] := TItemList.Create;
   FAttributes := TAttribList.Create;
   FConditions := TConditionList.Create;
   FBattleStyle := TBattleEngineList.Create(false);
   FMapEngines := TMapEngineList.Create(false);
   FLayout := TGameLayout.Create;
   FSwitches := TStringList.Create;
   FVariables := TStringList.Create;
   FFloats := TStringList.Create;
   FStrings := TStringList.Create;
   FSysVocab := TStringList.Create;
   FCustomVocab := TStringList.Create;
   FSerializer := TDatasetSerializer.Create;
   FLegacyData := TLegacyDataList.Create;
   FClass := TCharClassList.Create;
   FHero := THeroTemplateList.Create;
   FSkills := TSkillList.Create;
   FAnims := TAnimList.Create;
   FCommand := TBattleCommandList.Create;
   FTileGroup := TTileDictionary.Create([doOwnsValues]);
   FTileset := TTilesetList.Create;
   FGlobalEvents := TMapObjectList.Create;
   FVehicles := TVehicleList.Create;
   FMonsters := TMonsterList.Create;
   FMonsterParties := TMonsterPartyList.Create;
   FBattleChars := TBattleCharList.Create;
   FScriptLoaded := TSimpleEvent.Create;
   FScriptLoaded.ResetEvent;
end;

constructor TRpgDatabase.Load(dm: TDmDatabase);
var
   ds: TDataset;
   stream: TBytesStream;
   i, j: integer;
   dummy: string;
begin
   self.Create;

   ds := dm.dbData;
   FName := ds.FieldByName('name').AsString;
   FID := ds.FieldByName('id').AsInteger;
   FScriptFormat := TScriptFormat(ds.FieldByName('scriptFormat').AsInteger);
   FScriptFile := ds.FieldByName('scriptFile').AsString;

   TThread.CreateAnonymousThread(
      procedure
      var eventStream: TStream;
      begin
         try
            eventStream := GArchives[SCRIPT_ARCHIVE].getFile(FScriptFile);
            try
               FGlobalScriptBlock := TEBUnit.LoadFromStream(eventStream) as TEBUnit;
               FScriptLoaded.SetEvent;
            finally
               eventStream.Free;
            end;
         except
            on E: Exception do
            begin
               FreeAndNil(FScriptLoaded);
               FScriptLoadError := format('%s: %s', [E.ClassName, E.Message]);
            end;
         end;
      end).Start;

   FMapTree := TMapTree.Load(dm);

   stream := TBytesStream.Create(ds.FieldByName('statSet').AsBytes);
   try
      FStatSet := TStatSet.load(stream);
   finally
      stream.Free;
   end;

   stream := TBytesStream.Create(ds.FieldByName('moveMatrix').AsBytes);
   try
      SetLength(FMoveMatrix, stream.readWord);
      for I := 0 to high(FMoveMatrix) do
      begin
         SetLength(FMoveMatrix[i], stream.readWord);
         for j := 0 to high(FMoveMatrix[i]) do
         begin
            SetLength(FMoveMatrix[i,j], stream.readWord);
            stream.readBuffer(FMoveMatrix[i,j,0], length(FMoveMatrix[i,j]));
         end;
      end;
   finally
      stream.Free;
   end;

   stream := TBytesStream.Create(ds.FieldByName('battleStyles').AsBytes);
   try
      for I := 1 to stream.readByte do
      begin
         dummy := stream.readString; //order of operations issue
         FBattleStyle.Add(requireEngine(et_battle, dummy, TVersion.Create(stream.readInt)) as TBattleEngineData);
      end;
   finally
      stream.Free;
   end;

   stream := TBytesStream.Create(ds.FieldByName('mapStyles').AsBytes);
   try
      for I := 1 to stream.readByte do
      begin
         dummy := stream.readString; //order of operations issue
         FMapEngines.Add(requireEngine(et_map, dummy, TVersion.Create(stream.readInt)) as TMapEngineData);
      end;
   finally
      stream.Free;
   end;

   DownloadTileGroups(dm);
   FTileset.download(FSerializer, dm.tilesets);
end;

procedure TRpgDatabase.save(dm: TDmDatabase);
var
   ds: TDataset;
   stream: TBytesStream;
   i, j: integer;
   mapname: string;
begin
   ds := dm.dbData;
   assert(ds.RecordCount < 2);
   if ds.RecordCount = 0 then
      ds.Append
   else ds.Edit;

   ds.FieldByName('name').AsString := FName;
   ds.FieldByName('id').AsInteger := FID;

   FMapTree.SaveAll(dm);

   stream := TBytesStream.Create;
   try
      FStatSet.save(stream);
      ds.FieldByName('statSet').AsBytes := stream.Bytes;
   finally
      stream.Free;
   end;

   stream := TBytesStream.Create;
   try
      stream.writeWord(length(FMoveMatrix));
      for I := 0 to high(FMoveMatrix) do
      begin
         stream.writeWord(length(FMoveMatrix[i]));
         for j := 0 to high(FMoveMatrix[i]) do
         begin
            stream.writeWord(length(FMoveMatrix[i,j]));
            stream.WriteBuffer(FMoveMatrix[i,j,0], length(FMoveMatrix[i,j]));
         end;
      end;
      ds.FieldByName('moveMatrix').AsBytes := stream.Bytes;
   finally
      stream.Free;
   end;

   ds.FieldByName('scriptFormat').AsInteger := ord(FScriptFormat);
   ds.FieldByName('scriptFile').AsString := FScriptFile;

   stream := TBytesStream.Create;
   try
      stream.WriteByte(FBattleStyle.Count);
      for I := 0 to FBattleStyle.High do
      begin
         stream.writeString(FBattleStyle[i].name);
         stream.writeInt(FBattleStyle[i].version.value);
      end;
      ds.FieldByName('battleStyles').AsBytes := stream.Bytes;
   finally
      stream.Free;
   end;

   stream := TBytesStream.Create;
   try
      stream.WriteByte(FMapEngines.Count);
      for I := 0 to FMapEngines.High do
      begin
         stream.writeString(FMapEngines[i].name);
         stream.writeInt(FMapEngines[i].version.value);
      end;
      ds.FieldByName('mapStyles').AsBytes := stream.Bytes;
   finally
      stream.Free;
   end;
   ds.Post;

   mapname := FMapTree[FMapTree.currentMap].mapEngine;
   ds := dm.boot;
   assert(ds.RecordCount < 2);
   if ds.RecordCount = 0 then
      ds.Append
   else ds.Edit;
   ds.FieldByName('id').AsInteger := 0;
   ds.FieldByName('EngineName').AsString := mapname;
   ds.FieldByName('Version').AsInteger := requireEngine(et_map, mapname, TVersion.Create(0)).version.value;
   ds.Post;
end;

procedure TRpgDatabase.SaveScripts;
var
   stream: TStream;
begin
   stream := TStringStream.Create(FGlobalScriptBlock.Serialize, TEncoding.UTF8, false);
   try
      stream.rewind;
      GArchives[SCRIPT_ARCHIVE].writeFile(self.scriptFile, stream);
   finally
      stream.Free;
   end;
end;

procedure TRpgDatabase.SaveToDB(db: TdmDatabase);
var
   stream: TBytesStream;
begin
   stream := TBytesStream.Create;
   try
      Save(stream);
   finally
      stream.free;
   end;
end;

procedure TRpgDatabase.save;
begin
   SaveToDB(dmDatabase);
   dmDatabase.SaveAll;
   SaveScripts;
end;

destructor TRpgDatabase.Destroy;
var
   i: TItemType;
begin
   FScriptLoaded.Free;
   FLegacyData.Free;
   FBattleChars.Free;
   FMonsterParties.Free;
   FMonsters.Free;
   FVehicles.Free;
   FSerializer.Free;
   FSysVocab.Free;
   FCustomVocab.Free;
   FGlobalScriptBlock.Free;
   FGlobalEvents.Free;
   FSwitches.Free;
   FVariables.Free;
   FFloats.Free;
   FStrings.Free;
   FClass.Free;
   FHero.Free;
   FCommand.Free;
   FSkills.Free;
   for i := low(TItemType) to high(TItemType) do
      FItems[i].free;
   FAnims.Free;
   FConditions.Free;
   FAttributes.Free;
   FLayout.Free;
   FMapTree.Free;
   FStatSet.Free;
   FTileGroup.Free;
   FTileset.Free;
   FBattleStyle.Free;
   FMapEngines.Free;
   inherited Destroy;
end;

procedure TRpgDatabase.copyToDB(db: TdmDatabase; typeSet: TRpgDataTypeSet = [];
  report: TUploadReportProc = nil);
var
   i: TRpgDataTypes;
begin
   runThreadsafe(db.beginUpload, true);
   try
      if typeSet = [] then
      begin
         if assigned(report) then
            report('Preparing Data: system');
         self.save(db);
         typeSet := [low(TRpgDataTypes)..high(TRpgDataTypes)];
      end;
      for i in typeSet do
      begin
         if assigned(report) then
            report(format('Preparing Data: %s', [copy(typInfo.GetEnumName(typeinfo(TRpgDataTypes), ord(i)), 4, MAXINT)]));
         copyTypeToDB(db, i);
      end;
   finally
      runThreadsafe(db.endUpload, true);
   end;
end;

procedure TRpgDatabase.uploadStringList(dataset: TDataset; list: TStringList);
var
   i: integer;
   nameField: TWideStringField;
   idField: TIntegerField;
begin
   idField := dataset.FieldByName('id') as TIntegerField;
   nameField := dataset.FieldByName('name') as TWideStringField;
   dataset.DisableControls;
   try
      dataset.Append;
      idFIeld.Value := 0;
      nameField.Value := 'Local';
      dataset.Post;

      for I := 0 to List.Count - 1 do
      begin
         dataset.Append;
         idFIeld.Value := i + 1;
         nameField.Value := list[i];
         dataset.Post;
      end;
   finally
      dataset.EnableControls;
   end;
end;

procedure TRpgDatabase.UploadGlobalEvents(db: TdmDatabase);
var
   obj: TRpgMapObject;
   page: TRpgEventPage;
   ds: TDataset;
   id: integer;
   idField: TIntegerField;
   nameField: TWideStringField;
   condField: TIntegerField;
   hasSwitchField: TBooleanField;
   switchField: TIntegerField;
begin
   ds := db.GlobalScripts;
   ds.DisableControls;
   try
      idField := ds.FieldByName('id') as TIntegerField;
      nameField := ds.FieldByName('Name') as TWideStringField;
      condField := ds.FieldByName('StartCondition') as TIntegerField;
      hasSwitchField := ds.FieldByName('HasSwitch') as TBooleanField;
      switchField := ds.FieldByName('switch') as TIntegerField;
      id := 0;
      for obj in FGlobalEvents do
      begin
         inc(id);
         page := obj.pages.First;
         ds.Append;
         idField.Value := id;
         nameField.Value := page.scriptName;
         condField.Value := ord(page.startCondition);
         hasSwitchField.Value := pc_switch1 in page.conditionBlock.conditions;
         switchField.Value := page.conditionBlock.switch1Set;
         ds.Post;
      end;
   finally
      ds.EnableControls;
   end;
end;

procedure TRpgDatabase.UploadTileGroups(db: TdmDatabase);
var
   grp: TTileGroup;
begin
   for grp in FTileGroup.Values do
      grp.upload(FSerializer, db.tilegroups);
end;

procedure TRpgDatabase.DownloadTileGroups(db: TdmDatabase);
var
   grp: TTileGroup;
   row: variant;
begin
   for row in db.tilegroups do
   begin
      grp := TTileGroup.Create;
      grp.download(FSerializer, db.tilegroups);
      FTileGroup.Add(grp.filename, grp);
   end;
end;

procedure TRpgDatabase.UploadVocab(db: TdmDatabase);
var
   i: integer;
begin
   for I := 0 to FSysVocab.Count - 1 do
      db.Vocab.AppendRecord([FSysVocab.Names[i], FSysVocab.ValueFromIndex[i]]);
   for I := 0 to FCustomVocab.Count - 1 do
      db.CustomVocab.AppendRecord([FCustomVocab.Names[i], FCustomVocab.ValueFromIndex[i]]);
end;

procedure TRpgDatabase.copyTypeToDB(db: TdmDatabase; value: TRpgDataTypes);
var
   i: integer;
   enumerator: TRpgDatafile;
begin
   if value in FUploadedTypes then
      Exit;

   runThreadsafe(db.beginUpload, true);
   try
   case value of
      rd_class: FClass.upload(FSerializer, db.charClasses);
      rd_hero: FHero.upload(FSerializer, db.heroes);
      rd_command: FCommand.upload(FSerializer, db.commands);
      rd_item:
      begin
         for i := ord(low(TItemType)) to ord(high(TItemType)) do
         begin
            for enumerator in FItems[TItemType(i)] do
               if enumerator.id > 0 then
               begin
                  enumerator.upload(FSerializer, db.items);
                  db.items.FieldByName('itemType').AsInteger := i;
               end;
            db.items.postSafe;
         end;
      end;
      rd_skill: FSkills.upload(FSerializer, db.skills);
      rd_anim: FAnims.upload(FSerializer, db.animations);
      rd_attrib: FAttributes.upload(FSerializer, db.attributes);
      rd_condition: FConditions.upload(FSerializer, db.conditions);
      rd_tileset: FTileset.upload(FSerializer, db.tilesets);
      rd_vehicles: FVehicles.upload(FSerializer, db.vehicles);
      rd_monster: FMonsters.upload(FSerializer, db.monsters);
      rd_mParty: FMonsterParties.upload(FSerializer, db.mparties);
      rd_battleChar: FBattleChars.upload(FSerializer, db.battleChars);
      rd_switch: uploadStringList(db.Switches, FSwitches);
      rd_int: uploadStringList(db.Variables, FVariables);
      rd_float: uploadStringList(db.Floats, FFloats);
      rd_string: uploadStringList(db.Strings, FStrings);
      rd_vocab: UploadVocab(db);
      rd_script: UploadGlobalEvents(db);
      rd_tilegroup: UploadTileGroups(db);
      rd_layout: FLayout.upload(FSerializer, db.syslayout);
      rd_legacy:
      begin
         for enumerator in FLegacyData do
            enumerator.upload(FSerializer, db.LegacyData);
         db.LegacyData.postSafe;
      end;
      else assert(false);
   end;
   finally
      runThreadsafe(db.endUpload, true);
   end;

{   for dummy in db.datasets do
      dummy.First; }
   include(FUploadedTypes, value);
end;

function TRpgDatabase.findItem(id: integer; slot: TItemType): integer;
var
   i, check: integer;
begin
   result := -1;
   for I := 0 to FItems[slot].High do
   begin
      check := FItems[slot][i].id;
      if check = id then
         Exit(i)
      else if check > id then
         Exit;
   end;
end;

function TRpgDatabase.findItemById(id: integer): TItemTemplate;
var
   i: TItemType;
   iterator: TItemTemplate;
begin
   result := nil;
   for i := low(TItemType) to high(TItemType) do
   begin
      for iterator in FItems[i] do
      begin
         if iterator.id = id then
            Exit(iterator)
         else if iterator.id > id then
            Break;
      end;
   end;
end;

function TRpgDatabase.getClassCount: integer;
begin
   result := FClass.High;
end;

procedure TRpgDatabase.setClassCount(const value: integer);
var
   i: word;
begin
   i := FClass.Count;
   if value > i then
   begin
      for I := i to value do
         FClass.Add(TClassTemplate.Create);
   end
   else if value < i then
      FClass.DeleteRange(value + 1, i);
end;

procedure TRpgDatabase.setCommandCount(const Value: integer);
var
   i: word;
begin
   i := self.commands + 1;
   if value > i then
   begin
      for I := i + 1 to value do
         FCommand.add(TBattleCommand.Create);
   end
   else if value < i then
   begin
      for I := i downto value + 1 do
         FCommand.Delete(i);
   end;
end;

function TRpgDatabase.getHeroCount: integer;
begin
   result := FHero.High;
end;

function TRpgDatabase.GetMapTree: IMapTree;
begin
   result := FMapTree;
end;

function TRpgDatabase.getProjectName: string;
begin
   result := FMapTree[0].name;
end;

class function TRpgDatabase.keyChar: ansiChar;
begin
   result := #0;
   assert(false, 'Database files don''t require a key char.');
end;

procedure TRpgDatabase.setHeroCount(const Value: integer);
var
   i: word;
begin
   i := FHero.Count;
   if value > i then
   begin
      for I := i + 1 to value do
         AddHero;
   end
   else if value < i then
      FHero.DeleteRange(value + 1, i);
end;

procedure TRpgDatabase.SetScriptFormat(const Value: TScriptFormat);
begin
   FScriptFormat := Value;
   // do more here
end;

function TRpgDatabase.sumItems: integer;
var
   i: TItemType;
begin
   result := 0;
   for i := low(TItemType) to high(TItemType) do
      inc(result, FItems[i].High);
end;

function TRpgDatabase.getCommandCount: integer;
begin
   result := FCommand.High;
end;

procedure TRpgDatabase.addAnim;
begin
   FAnims.add(TAnimTemplate.Create);
end;

procedure TRpgDatabase.addAttribute;
begin
   FAttributes.add(TAttributeTemplate.Create);
end;

procedure TRpgDatabase.addClass;
begin
   FClass.Add(TClassTemplate.Create);
end;

procedure TRpgDatabase.addCondition;
begin
   FConditions.add(TConditionTemplate.Create);
end;

procedure TRpgDatabase.addHero;
begin
   FHero.Add(THeroTemplate.Create);
end;

{$WARN USE_BEFORE_DEF OFF}
procedure TRpgDatabase.addItem(slot: TItemType);
var
   newItem: TItemTemplate;
begin
   case slot of
      it_junk: newItem := TJunkTemplate.Create;
      it_weapon: newItem := TWeaponTemplate.Create;
      it_armor: newItem := TArmorTemplate.Create;
      it_medicine: newItem := TMedicineTemplate.Create;
      it_upgrade: newItem := TStatItemTemplate.Create;
      it_book: newItem := TSkillBookTemplate.Create;
      it_skill: newItem := TSkillItemTemplate.Create;
      it_variable: newItem := TVariableItemTemplate.Create;
      it_script: newItem := TScriptItemTemplate.Create;
      else assert(false);
   end;
   FItems[slot].Add(newItem);
end;
{$WARN USE_BEFORE_DEF ON}

procedure TRpgDatabase.AddLegacy(const name: string; id, section: integer;
  const data: RawByteString);
begin
   FLegacyData.Add(TLegacyData.Create(name, id, section, data));
end;

procedure TRpgDatabase.addSkill;
begin
   FSkills.Add(TSkillTemplate.Create);
end;

procedure TRpgDatabase.addTileset;
begin
   FTileset.Add(TTileset.Create);
end;

function TRpgDatabase.countItems: cardinal;
var i: TItemType;
begin
   result := 0;
   for i := low(TItemType) to high(TItemType) do
      inc(result, FItems[i].High);
end;

{ TBattleCommandList }

function TBattleCommandList.indexOf(name: string): integer;
var
   enumerator: TBattleCommand;
begin
   result := -1;
   for enumerator in self do
      if enumerator.name = name then
         Exit(enumerator.id);
end;

initialization
begin
   GDatabase := nil;
end;

finalization
begin
   GDatabase.Free;
   GScriptEngine := nil;
end;

end.
