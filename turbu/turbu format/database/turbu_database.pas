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
   classes, sysUtils, Generics.Collections, DB,
//   events,
   dm_database, turbu_database_interface, turbu_map_interface, EB_RpgScript,
   turbu_characters, turbu_items, turbu_skills, turbu_classes, turbu_resists,
   turbu_battle_engine, turbu_map_engine, turbu_sprites, turbu_animations,
   turbu_unit_dictionary, turbu_containers, turbu_script_interface, turbu_game_data,
   turbu_map_metadata, turbu_tilesets, turbu_decl_utils, turbu_maps,
   turbu_serialization;

type
   TCharClassList = TRpgObjectList<TClassTemplate>;
   THeroTemplateList = TRpgObjectList<THeroTemplate>;
   TBattleEngineList = TRpgObjectList<TBattleEngineData>;
   TMapEngineList = TRpgObjectList<TMapEngineData>;
   TMoveMatrixArray = array of TMoveMatrix;
   TItemList = TRpgObjectList<TItemTemplate>;
   TItemMatrix = array[TItemType] of TItemList;
   TSkillList = TRpgObjectList<TSkillTemplate>;
   TAnimList = TRpgObjectList<TAnimTemplate>;
   TAttribList = TRpgObjectList<TAttributeTemplate>;
   TConditionList = TRpgObjectList<TConditionTemplate>;
   TTilesetList = TRpgObjectList<TTileset>;
   TTileDictionary = TObjectDictionary<string, TTileGroup>;
   TScriptList = TRpgObjectList<TScriptRecord>;

   TRpgDataTypes = (rd_class, rd_hero, rd_command, rd_item, rd_skill, rd_anim,
                    rd_attrib, rd_condition, rd_tileset, rd_switch, rd_int,
                    rd_float, rd_string, rd_script, rd_metadata, rd_vocab);
   TRpgDataTypeSet = set of TRpgDataTypes;

   TBattleCommandList = class(TRpgObjectList<TBattleCommand>)
   public
      function indexOf (name: string): integer;
   end;

   TMapLoadProc = reference to procedure(tree: IMapTree);

   TLegacySections = class(TDictionary<word, rawbytestring>);

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

      //I need monsters and monster parties
      //and battle anims and terrains
      //and system data
      //and a battle layout section
      //and whatever $20 is in 2003

      FGlobalEvents: TMapObjectList;
      FLayout: TGameLayout;
      FSwitches: TStringList;
      FVariables: TStringList;
      FFloats: TStringList;
      FStrings: TStringList;
      FScripts: TScriptList;

      FUnits: TUnitDictionary;
      FSkillAlgs: TStringList;
      FStatAlgs: TStringList;
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

      FFilename: string;
      procedure SetScriptFormat(const Value: TScriptFormat);
      function getClassCount: integer;
      procedure setClassCount(const Value: integer);
      procedure parseMeta;
      function getCommandCount: integer;
      procedure setCommandCount(const Value: integer);
      function getHeroCount: integer;
      procedure setHeroCount(const Value: integer);

      function countItems: cardinal;
      procedure prepareBlanks;
      function sumItems: integer;
      procedure setUnits(const Value: TUnitDictionary);
      function getProjectName: string;

      procedure saveTileGroups(savefile: TStream);
      procedure saveTilesets(savefile: TStream);
      procedure uploadStringList(dataset: TDataset; list: TStringList);
      procedure UploadGlobalEvents(db: TdmDatabase);
      procedure UploadVocab(db: TdmDatabase);
   private
      FSerializer: TDatasetSerializer;
   protected
      FLegacyCruft: TLegacySections;
      FGlobalScriptBlock: TEBUnit;
      FSysVocab: TStringList;
      FCustomVocab: TStringList;
      class function keyChar: ansiChar; override;
   public
      constructor Create;
      constructor Load(savefile: TStream; design: boolean; onLoadMap: TMapLoadProc; onStageCount: TProc<integer>); reintroduce;
      destructor Destroy; override;
      procedure save(savefile: TStream); overload; override;
      procedure save; reintroduce; overload;
      procedure copyToDB(db: TdmDatabase; typeSet: TRpgDataTypeSet = []);
      procedure copyTypeToDB(db: TdmDatabase; value: TRpgDataTypes);
      procedure loadFromDB(value: TdmDatabase);

      procedure AfterConstruction; override;

      procedure addClass; overload;
      procedure addClass(value: TClassTemplate); overload;

      procedure addHero; overload;
      procedure addHero(value: THeroTemplate); overload;

      procedure addItem(slot: TItemType); overload;
      procedure addItem(value: TItemTemplate; slot: TItemType); overload;
      function findItem(id: integer; slot: TItemType): integer;
      function findItemById(id: integer): TItemTemplate;

      procedure addSkill; overload;
      procedure addSkill(value: TSkillTemplate); overload;

      procedure addAnim; overload;
      procedure addAnim(value: TAnimTemplate); overload;

      procedure addAttribute;
      procedure addCondition;
      procedure addTileset;

      function scriptBuild: boolean;
      function scriptByName(name: string): TScriptRecord;

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
      property tileGroup: TTileDictionary read FTileGroup write FTileGroup;
      property scripts: TScriptList read FScripts write FScripts;
      property scriptFormat: TScriptFormat read FScriptFormat write SetScriptFormat;
      property scriptFile: string read FScriptFile write FScriptFile;

      property units: TUnitDictionary read FUnits write setUnits;
      property projectName: string read getProjectName;
      property uploadedTypes: TRpgDataTypeSet read FUploadedTypes;
      property Filename: string read FFilename write FFilename;
      property serializer: TDatasetSerializer read FSerializer;
   end;

var
   GDatabase: TRpgDatabase;
   GScriptEngine: IScriptEngine;

implementation
uses
   zlib, math, TypInfo,
   archiveInterface, commons,
   turbu_constants, turbu_engines, turbu_versioning, turbu_plugin_interface,
   turbu_functional, turbu_map_objects;

const
   MIN_DBVERSION = 38;
   DBVERSION = 38;

{ TRpgDatabase }

constructor TRpgDatabase.Create;
var
   i: TItemType;
begin
   inherited Create;
   FName := 'TURBU RPG Database';
   FID := DBVERSION;
   FUnits := TUnitDictionary.Create(20);
   FStatAlgs := TStringList.Create;
   FSkillAlgs := TStringList.Create;
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
   self.prepareBlanks;
end;

constructor TRpgDatabase.Load(savefile: TStream; design: boolean; onLoadMap: TMapLoadProc; onStageCount: TProc<integer>);
type
   TItemClass = class of TItemTemplate;
   TSkillClass = class of TSkillTemplate;
const
   items: array[TItemType] of TItemClass =
   (TJunkTemplate, TWeaponTemplate, TArmorTemplate, TMedicineTemplate,
   TStatItemTemplate, TSkillBookTemplate, TSkillItemTemplate,
   TVariableItemTemplate, TScriptItemTemplate);

   procedure loadStringList(stream: TStream; var list: TStringList);
   var
      newstream: TStringStream;
      data: string;
      size: integer;
   begin
      size := stream.readInt;
      data := stream.readString;
      newStream := TStringStream.Create(data);
      try
         newStream.Position := 0;
         list := TStringList.Create;
         list.LoadFromStream(newStream);
         lassert(list.Count = size);
      finally
         newStream.Free;
      end;
   end;

var
   substream: TStream;
   i, j, k: integer;
   item: TItemType;
   skills: TSkillClass;
   dummy: string;
   filename, uFilename: string;
   EventLoader: TThread;
begin
   skills := nil;
   for item := low(TItemType) to high(TItemType) do
      FItems[item] := TItemList.Create;
   FAttributes := TAttribList.Create;
   FConditions := TConditionList.Create;
   FBattleStyle := TBattleEngineList.Create(false);
   FMapEngines := TMapEngineList.Create(false);
   self.prepareBlanks;
   GDatabase := self;

   inherited load(savefile);
   lassert(FName = 'TURBU RPG Database');
   lassert(FID = DBVERSION);

   if assigned(onStageCount) then
      onStageCount(ord(high(TRpgDataTypes)));

   substream := GArchives[DATABASE_ARCHIVE].getFile('legacy.tdf');
   try
      while not substream.eof do
      begin
         i := subStream.readWord;
         FLegacyCruft.Add(i, subStream.readAString);
      end;
      lassert(savefile.readChar = 'L');
      lassert(FLegacyCruft.Count = savefile.readInt);
      lassert(savefile.readChar = 'l');
   finally
      substream.free;
   end;

   substream := GArchives[DATABASE_ARCHIVE].getFile('mapTree.tdf');
   try
      FMapTree := TMapTree.Load(subStream);
      lassert(savefile.readChar = 'T');
      lassert(FMapTree.Count = savefile.readInt);
      lassert(savefile.readChar = 't');
   finally
      substream.free;
   end;

   lassert(savefile.readChar = 'L');
   FLayout := TGameLayout.Load(savefile);
   lassert(savefile.readChar = 'l');

   lassert(savefile.readChar ='G');
   for i := 1 to savefile.readInt do
   begin
      dummy := savefile.readString;
      FTileGroup.Add(dummy, TTileGroup.Load(savefile));
   end;
//   savefile.readDict<TTileGroup>(FTileGroup);
   lassert(savefile.readChar = 'g');

   lassert(savefile.readChar = 'T');
   for i := 1 to savefile.readInt do
      FTileSet.Add(TTileSet.Load(savefile));

//   savefile.readDict<TTileset>(FTileset);
   lassert(savefile.readChar = 't');

   lassert(savefile.readChar = 'S');
   FStatSet.free;
   FStatSet := TStatSet.load(savefile);
   lassert(savefile.readChar = 's');

   substream := GArchives[DATABASE_ARCHIVE].getFile('lists.tdf');
   try
      lassert(subStream.readChar = 'L');
      loadStringList(subStream, FSkillAlgs);
      loadStringList(subStream, FStatAlgs);
      loadStringList(substream, FSwitches);
      loadStringList(substream, FVariables);
      loadStringList(substream, FFloats);
      loadStringList(substream, FStrings);
      loadStringList(substream, FSysVocab);
      loadStringList(substream, FCustomVocab);
      lassert(subStream.readChar = 'l');
   finally
      substream.free;
   end;
   lassert(savefile.readInt = FSkillAlgs.Count);
   lassert(savefile.readInt = FStatAlgs.Count);

   lassert(savefile.readChar = 'M');
   setLength(FMoveMatrix, savefile.readWord);
   for I := 0 to high(FMoveMatrix) do
   begin
      setLength(FMoveMatrix[i], savefile.readWord);
      for j := 0 to high(FMoveMatrix[i]) do
      begin
         setLength(FMoveMatrix[i,j], savefile.readWord);
         savefile.readBuffer(FMoveMatrix[i,j,0], length(FMoveMatrix[i,j]));
      end;
   end;
   lassert(savefile.readChar = 'm');

   if assigned(onLoadMap) then
   begin
      commons.runThreadsafe(
      procedure
      begin
         onLoadMap(FMapTree);
      end, true);
   end;

   EventLoader := TThread.CreateAnonymousThread(
      procedure
      var eventStream: TStream;
      begin
         eventStream := GArchives[SCRIPT_ARCHIVE].getFile(format('%s.trs', [self.name]));
         try
            FGlobalScriptBlock := TEBUnit.LoadFromStream(eventStream) as TEBUnit;
         finally
            eventStream.Free;
         end;
      end);
   EventLoader.FreeOnTerminate := false;
   EventLoader.Start;

   savefile.ReadBuffer(FScriptFormat, sizeof(FScriptFormat));
   FScriptFile := savefile.readString;
   k := savefile.readInt;
   FUnits := TUnitDictionary.Create(k * 2);
   for filename in GArchives[DATABASE_ARCHIVE].allFiles('scripts') do
   begin
      substream := GArchives[DATABASE_ARCHIVE].getFile(filename);
      try
         uFilename := upperCase(ExtractFileName(filename));
         uFilename := stringReplace(uFilename, '.TRS', '', []);
         FUnits.Add(uFileName, TStringList.Create);
         FUnits[uFileName].LoadFromStream(substream);
      finally
         substream.Free;
      end;
   end;
   lassert(k = FUnits.Count);
   if design then
   begin
      GScriptEngine.units := FUnits;
      self.scriptBuild;
      self.parseMeta;
   end;

   GArchives[DATABASE_ARCHIVE].CurrentFolder := '';
   j := savefile.readInt;
   if j > 0 then
   begin
      FClass.Capacity := j + 1;
      substream := GArchives[DATABASE_ARCHIVE].getFile('classes.tdf');
      try
         lassert(subStream.readChar = 'C');
         lassert(subStream.readInt = j);
         for I := 1 to j do
            FClass.Add(TClassTemplate.Load(subStream));
         lassert(subStream.readChar = 'c');
      finally
         substream.Free;
      end;
   end;

   j := savefile.readInt;
   if j > 0 then
   begin
      FHero.Capacity := j + 1;
      substream := GArchives[DATABASE_ARCHIVE].getFile('heroes.tdf');
      try
         lassert(subStream.readChar = 'H');
         lassert(subStream.readInt = j);
         for I := 1 to j do
            FHero.Add (THeroTemplate.Load(subStream));
         lassert(subStream.readChar = 'h');
      finally
         substream.Free;
      end;
   end;

   FCommand.Add(TBattleCommand.Create);
   j := savefile.readInt;
   if j > 0 then
   begin
      substream := GArchives[DATABASE_ARCHIVE].getFile('battleCommands.tdf');
      try
         lassert(subStream.readChar = 'B');
         lassert(subStream.readInt = j);
         for I := 1 to j do
            FCommand.Add(TBattleCommand.Load(subStream));
         lassert(subStream.readChar = 'b');
      finally
         substream.Free;
      end;
   end;

   k := savefile.readInt;
   if k > 0 then
   begin
      substream := GArchives[DATABASE_ARCHIVE].getFile('items.tdf');
      try
         lassert(subStream.readChar = 'I');
         lassert(subStream.readInt = k);
         for item := low(TItemType) to high(TItemType) do
            for i := 1 to subStream.readInt do
               FItems[item].Add(items[item].Load(subStream));
         lassert(subStream.readChar = 'i');
      finally
         substream.Free;
      end;
   end;

   j := savefile.readInt;
   if j > 0 then
   begin
      FSkills.Capacity := j + 1;
      substream := GArchives[DATABASE_ARCHIVE].getFile('skills.tdf');
      try
         lassert(subStream.readChar = 'S');
         lassert(subStream.readInt = j);
         for I := 1 to j do
         begin
            case subStream.readChar of
               'N': skills := TNormalSkillTemplate;
               'V': skills := TVariableSkillTemplate;
               'T': skills := TTeleportSkillTemplate;
               else lassert(false);
            end;
            FSkills.add(skills.Load(subStream));
         end;
         lassert(subStream.readChar = 's');
      finally
         substream.Free;
      end;
   end;

   j := savefile.readInt;
   if j > 0 then
   begin
      FAnims.Capacity := j + 1;
      substream := GArchives[DATABASE_ARCHIVE].getFile('animations.tdf');
      try
         lassert(subStream.readChar = 'A');
         lassert(subStream.readInt = j);
         for I := 1 to j do
            FAnims.Add(TAnimTemplate.Load(subStream));
         lassert(subStream.readChar = 'a');
      finally
         substream.Free;
      end;
   end;

   k := savefile.readInt;
   if k > 0 then
   begin
      substream := GArchives[DATABASE_ARCHIVE].getFile('attributes.tdf');
      try
         lassert(subStream.readChar = 'A');
         lassert(subStream.readInt = k);
         for I := 1 to k do
            FAttributes.add(TAttributeTemplate.Load(subStream));
         lassert(subStream.readChar = 'a');
      finally
         substream.Free;
      end;
   end;

   k := savefile.readInt;
   if k > 0 then
   begin
      substream := GArchives[DATABASE_ARCHIVE].getFile('conditions.tdf');
      try
         lassert(subStream.readChar = 'C');
         lassert(subStream.readInt = k);
         for I := 1 to k do
            FConditions.Add(TConditionTemplate.Load(subStream));
         lassert(subStream.readChar = 'c');
      finally
         subStream.free;
      end;
   end;

   lassert(savefile.readChar = 'B');
   FBattleStyle.Capacity := savefile.readByte;
   for I := 0 to FBattleStyle.Capacity - 1 do
   begin
      dummy := savefile.readString; //order of operations issue
      FBattleStyle.Add(requireEngine(et_battle, dummy, TVersion.Create(savefile.readInt)) as TBattleEngineData);
   end;
   lassert(savefile.readChar = 'b');

   lassert(savefile.readChar = 'M');
   FMapEngines.Capacity := savefile.readByte;
   for I := 0 to FMapEngines.Capacity - 1 do
   begin
      dummy := savefile.readString; //order of operations issue
      FMapEngines.Add(requireEngine(et_map, dummy, TVersion.Create(savefile.readInt)) as TMapEngineData);
   end;
   lassert(savefile.readChar = 'm');

   lassert(savefile.readChar = 'G');
   FGlobalEvents.Capacity := savefile.readInt;
   for i := 0 to FGlobalEvents.Capacity - 1 do
      FGlobalEvents.Add(TRpgMapObject.Load(savefile));
   lassert(savefile.readChar = 'g');

   lassert(savefile.readChar = 'd');
   EventLoader.WaitFor;
   EventLoader.Free;
end;

procedure TRpgDatabase.save(savefile: TStream);
var
   substream: TStream;
   i, j: integer;
   item: TItemType;
   iterator: TRpgDatafile;
   filename: string;

   procedure saveStringList(stream: TStream; list: TStringList);
   var
      newStream: TStringStream;
   begin
      stream.writeInt(list.Count);
      newStream := TStringStream.Create('');
      try
         list.SaveToStream(newStream);
         stream.writeString(newStream.DataString);
      finally
         newStream.Free;
      end;
   end;

var
   enumerator: TPair<word, rawbytestring>;
begin
   inherited save(savefile);

   substream := TMemoryStream.Create;
   try
      for enumerator in FLegacyCruft do
      begin
         subStream.writeWord(enumerator.key);
         subStream.writeAString(enumerator.value);
      end;
      savefile.writeChar('L');
      savefile.writeInt(FLegacyCruft.Count);
      savefile.writeChar('l');
      GArchives[DATABASE_ARCHIVE].writeFile('legacy.tdf', substream);
   finally
      substream.free;
   end;

   substream := TMemoryStream.Create;
   try
      FMapTree.save(subStream);
      savefile.writeChar('T');
      savefile.writeInt(FMapTree.Count);
      savefile.writeChar('t');
      GArchives[DATABASE_ARCHIVE].writeFile('mapTree.tdf', subStream);
   finally
      substream.free;
   end;

   savefile.writeChar('L');
   FLayout.save(savefile);
   savefile.writeChar('l');

   savefile.writeChar('G');
   saveTileGroups(savefile);
//   savefile.writeDict<TTileGroup>(FTileGroup);
   savefile.writeChar('g');

   savefile.writeChar('T');
   saveTilesets(savefile);
//   savefile.writeDict<TTileset>(FTileset);
   savefile.writeChar('t');

   savefile.writeChar('S');
   FStatSet.save(savefile);
   savefile.writeChar('s');

   savefile.writeInt(FSkillAlgs.Count);
   savefile.writeInt(FStatAlgs.Count);
   substream := TMemoryStream.Create;
   try
      substream.writeChar('L');
      saveStringList(substream, FSkillAlgs);
      saveStringList(substream, FStatAlgs);
      saveStringList(substream, FSwitches);
      saveStringList(substream, FVariables);
      saveStringList(substream, FFloats);
      saveStringList(substream, FStrings);
      saveStringList(substream, FSysVocab);
      saveStringList(substream, FCustomVocab);
      substream.writeChar('l');
      GArchives[DATABASE_ARCHIVE].writeFile('lists.tdf', substream);
   finally
      substream.free;
   end;

   savefile.writeChar('M');
   savefile.writeWord(length(FMoveMatrix));
   for I := 0 to high(FMoveMatrix) do
   begin
      savefile.writeWord(length(FMoveMatrix[i]));
      for j := 0 to high(FMoveMatrix[i]) do
      begin
         savefile.writeWord(length(FMoveMatrix[i,j]));
         savefile.WriteBuffer(FMoveMatrix[i,j,0], length(FMoveMatrix[i,j]));
      end;
   end;
   savefile.writeChar('m');

   savefile.WriteBuffer(FScriptFormat, sizeof(FScriptFormat));
   savefile.writeString(FScriptFile);
   savefile.writeInt(FUnits.Count);
   for filename in FUnits.Keys do
   begin
      subStream := TMemoryStream.Create;
      try
         FUnits[filename].SaveToStream(substream);
         GArchives[DATABASE_ARCHIVE].writeFile(format('scripts\%s.trs', [FUnits.original(filename)]), substream);
      finally
         substream.Free;
      end;
   end;

   savefile.writeInt(FClass.High);
   if FClass.High > 0 then
   begin
      subStream := TMemoryStream.Create;
      try
         subStream.writeChar('C');
         subStream.writeInt(FClass.High);
         for I := 1 to FClass.High do
            FClass[i].save(substream);
         subStream.writeChar('c');
         GArchives[DATABASE_ARCHIVE].writeFile('classes.tdf', substream);
      finally
         substream.Free;
      end;
   end;

   savefile.writeInt(FHero.High);
   if FHero.High > 0 then
   begin
      subStream := TMemoryStream.Create;
      try
         subStream.writeChar('H');
         subStream.writeInt(FHero.High);
         for I := 1 to FHero.High do
            FHero[i].save(substream);
         subStream.writeChar('h');
         GArchives[DATABASE_ARCHIVE].writeFile('heroes.tdf', substream);
      finally
         substream.Free;
      end;
   end;

   savefile.writeInt(FCommand.high);
   if FCommand.High > 0 then
   begin
      subStream := TMemoryStream.Create;
      try
         subStream.writeChar('B');
         subStream.writeInt(FCommand.High);
         for I := 1 to FCommand.High do
            FCommand[i].save(substream);
         subStream.writeChar('b');
         GArchives[DATABASE_ARCHIVE].writeFile('battleCommands.tdf', substream);
      finally
         substream.Free;
      end;
   end;

   savefile.writeInt(countItems);
   if countItems > 0 then
   begin
      subStream := TMemoryStream.Create;
      try
         subStream.writeChar('I');
         subStream.writeInt(countItems);
         for item := low(TItemType) to high(TItemType) do
         begin
            subStream.writeInt(FItems[item].high);
            for I := 1 to FItems[item].High do
               FItems[item][i].save(substream);
         end;
         subStream.writeChar('i');
         GArchives[DATABASE_ARCHIVE].writeFile('items.tdf', substream);
      finally
         substream.Free;
      end;
   end;

   savefile.writeInt(FSkills.High);
   if FSkills.High > 0 then
   begin
      subStream := TMemoryStream.Create;
      try
         subStream.writeChar('S');
         subStream.writeInt(FSkills.High);
         for I := 1 to FSkills.High do
            FSkills[i].save(substream);
         subStream.writeChar('s');
         GArchives[DATABASE_ARCHIVE].writeFile('skills.tdf', substream);
      finally
         substream.Free;
      end;
   end;

   savefile.writeInt(FAnims.High);
   if FAnims.High > 0 then
   begin
      subStream := TMemoryStream.Create;
      try
         subStream.writeChar('A');
         subStream.writeInt(FAnims.High);
         for I := 1 to FAnims.High do
            FAnims[i].save(substream);
         subStream.writeChar('a');
         GArchives[DATABASE_ARCHIVE].writeFile('animations.tdf', substream);
      finally
         substream.Free;
      end;
   end;

   savefile.writeInt(FAttributes.High);
   if FAttributes.High > 0 then
   begin
      subStream := TMemoryStream.Create;
      try
         subStream.writeChar('A');
         subStream.writeInt(FAttributes.High);
         for iterator in FAttributes do
            if iterator.id > 0 then
               iterator.save(substream);
         subStream.writeChar('a');
         GArchives[DATABASE_ARCHIVE].writeFile('attributes.tdf', substream);
      finally
         substream.Free;
      end;
   end;

   savefile.writeInt(FConditions.high);
   if FConditions.high > 0 then
   begin
      subStream := TMemoryStream.Create;
      try
         subStream.writeChar('C');
         subStream.writeInt(FConditions.High);
         for I := 1 to FConditions.High do
            FConditions[i].save(substream);
         subStream.writeChar('c');
         GArchives[DATABASE_ARCHIVE].writeFile('conditions.tdf', substream);
      finally
         substream.Free;
      end;
   end;

   savefile.writeChar('B');
   savefile.WriteByte(FBattleStyle.Count);
   for I := 0 to FBattleStyle.High do
   begin
      savefile.writeString(FBattleStyle[i].name);
      savefile.writeInt(FBattleStyle[i].version.value);
   end;
   savefile.writeChar('b');

   savefile.writeChar('M');
   savefile.WriteByte(FMapEngines.Count);
   for I := 0 to FMapEngines.High do
   begin
      savefile.writeString(FMapEngines[i].name);
      savefile.writeInt(FMapEngines[i].version.value);
   end;
   savefile.writeChar('m');

   savefile.writeChar('G');
   savefile.writeInt(FGlobalEvents.Count);
   for i := 0 to FGlobalEvents.High do
      FGlobalEvents[i].save(savefile);
   savefile.writeChar('g');

   subStream := TStringStream.Create(FGlobalScriptBlock.Serialize, TEncoding.UTF8, false);
   try
      subStream.rewind;
      GArchives[SCRIPT_ARCHIVE].writeFile(format('%s.trs', [self.name]), subStream);
   finally
      subStream.Free;
   end;

   savefile.writeChar('d');
end;

procedure TRpgDatabase.save;
var
   stream, filestream: TStream;
begin
   filestream := nil;
   stream := TMemoryStream.Create;
   try
      Save(stream);
      filestream := TFileStream.Create(FFilename, fmCreate);
      stream.Rewind;
      filestream.CopyFrom(stream, stream.Size);
   finally
      filestream.Free;
      stream.free;
   end;
end;

procedure TRpgDatabase.saveTileGroups(savefile: TStream);
var
   enumerator: TPair<string, TTileGroup>;
begin
   savefile.writeInt(FTileGroup.Count);
   for enumerator in FTileGroup do
   begin
      savefile.writeString(enumerator.Key);
      enumerator.Value.save(savefile);
   end;
end;

procedure TRpgDatabase.saveTilesets(savefile: TStream);
var
   enumerator: TTileSet;
begin
   savefile.writeInt(FTileSet.High);
   for enumerator in FTileSet do
      if enumerator.id > 0 then
         enumerator.save(savefile);
end;

function TRpgDatabase.scriptBuild: boolean;
const
   SCRIPT = 'program buildAll;' + LFCR + LFCR +
            'uses' + LFCR +
            '   %s;' + LFCR + LFCR +
            'begin' + LFCR +
            'end.';
var
   list: TStringList;
   item: string;
begin
   result := true;
   list := TStringList.Create;
   try
      for item in FUnits.Keys do
         list.Add(item);
      try
         GScriptEngine.dbScript := ansiString(format(SCRIPT, [list.CommaText]));
      except
         on E: Exception do
         begin
            result := false;
            MsgBox('See the log file for details.', e.Message);
            Abort;
         end;
      end;
   finally
      list.Free;
   end;
end;

function TRpgDatabase.scriptByName(name: string): TScriptRecord;
begin
   result := self.FScripts.firstWhere(
      function(Arg1: TScriptRecord): boolean
      begin
         result := arg1.name = name;
      end);
end;

destructor TRpgDatabase.Destroy;
var
   i: TItemType;
begin
   FSerializer.Free;
   FSysVocab.Free;
   FCustomVocab.Free;
   FGlobalScriptBlock.Free;
   FGlobalEvents.Free;
   FScripts.Free;
   FLegacyCruft.Free;
   FSwitches.Free;
   FVariables.Free;
   FFloats.Free;
   FStrings.Free;
   FUnits.Free;
   FStatAlgs.Free;
   FClass.Free;
   FHero.Free;
   FCommand.Free;
   FSkills.Free;
   if assigned(FSkillAlgs) then
      FSkillAlgs.OwnsObjects := true;
   FSkillAlgs.Free;
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

procedure TRpgDatabase.copyToDB(db: TdmDatabase; typeSet: TRpgDataTypeSet = []);
var
   i: TRpgDataTypes;
begin
   runThreadsafe(db.beginUpload, true);
   try
      if typeSet = [] then
         typeSet := [low(TRpgDataTypes)..high(TRpgDataTypes)];
      for i in typeSet do
         copyTypeToDB(db, i);
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
   eventField: TWideMemoField;
   condField: TIntegerField;
   hasSwitchField: TBooleanField;
   switchField: TIntegerField;
begin
   ds := db.GlobalScripts;
   ds.DisableControls;
   try
      idField := ds.FieldByName('id') as TIntegerField;
      nameField := ds.FieldByName('Name') as TWideStringField;
      eventField := ds.FieldByName('EventText') as TWideMemoField;
      condField := ds.FieldByName('StartCondition') as TIntegerField;
      hasSwitchField := ds.FieldByName('HasSwitch') as TBooleanField;
      switchField := ds.FieldByName('switch') as TIntegerField;
      id := 0;
      for obj in self.globalEvents do
      begin
         inc(id);
         page := obj.pages.First;
         ds.Append;
         idField.Value := id;
         nameField.Value := page.scriptName;
         eventField.Value := (FGlobalScriptBlock.FindComponent(page.scriptName) as TEBProcedure).Serialize;
         condField.Value := ord(page.startCondition);
         hasSwitchField.Value := pc_switch1 in page.conditionBlock.conditions;
         switchField.Value := page.conditionBlock.switch1Set;
         ds.Post;
      end;
   finally
      ds.EnableControls;
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
   dummy: TDataSet;
begin
   if value in FUploadedTypes then
      Exit;

   runThreadsafe(db.beginUpload, true);
   try
   case value of
      rd_class:
      begin
         for enumerator in FClass do
            if enumerator.id > 0 then
               enumerator.upload(FSerializer, db.charClasses);
         db.charClasses.postSafe;
      end;
      rd_hero:
      begin
         for enumerator in FHero do
            if enumerator.id > 0 then
               enumerator.upload(FSerializer, db.heroes);
         db.heroes.postSafe;
      end;
      rd_command:
      begin
         for enumerator in FCommand do
            if enumerator.id > 0 then
               enumerator.upload(FSerializer, db.commands);
         db.commands.postSafe;
      end;
      rd_item:
      begin
         for i := ord(low(TItemType)) to ord(high(TItemType)) do
         begin
            dummy := db.FindComponent('items') as TDataset;
            for enumerator in FItems[TItemType(i)] do
               if enumerator.id > 0 then
               begin
                  enumerator.upload(FSerializer, dummy);
                  dummy.FieldByName('itemType').AsInteger := i;
               end;
            dummy.postSafe;
         end;
      end;
      rd_skill:
      begin
         for enumerator in FSkills do
            if enumerator.id > 0 then
               enumerator.upload(FSerializer, db.skills);
         db.skills.postSafe;
      end;
      rd_anim:
      begin
         for enumerator in FAnims do
            if enumerator.id > 0 then
               enumerator.upload(FSerializer, db.animations);
         db.animations.postSafe;
      end;
      rd_attrib:
      begin
         for I := 1 to FAttributes.High do
            FAttributes[i].upload(FSerializer, db.attributes);
         db.attributes.postSafe;
      end;
      rd_condition:
      begin
         for I := 1 to FConditions.High do
            FConditions[i].upload(FSerializer, db.conditions);
         db.conditions.postSafe;
      end;
      rd_tileset:
      begin
         for i := 1 to FTileset.High do
            FTileset[i].upload(FSerializer, db.tilesets);
         db.tilesets.postSafe;
      end;
      rd_switch: uploadStringList(db.Switches, FSwitches);
      rd_int: uploadStringList(db.Variables, FVariables);
      rd_float: uploadStringList(db.Floats, FFloats);
      rd_string: uploadStringList(db.Strings, FStrings);
      rd_script:
      begin
         (GScriptEngine as IDesignScriptEngine).upload(db);
         UploadGlobalEvents(db);
      end;
      rd_metadata:
      begin
         for enumerator in FMapTree do
            enumerator.upload(FSerializer, db.metadata);
         db.metadata.postSafe;
      end;
      rd_vocab: UploadVocab(db);
      else assert(false);
   end;
   finally
      runThreadsafe(db.endUpload, true);
   end;

{   for dummy in db.datasets do
      dummy.First; }
   include(FUploadedTypes, value);
end;

procedure TRpgDatabase.loadFromDB(value: TdmDatabase);
begin

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

procedure TRpgDatabase.setUnits(const Value: TUnitDictionary);
begin
   FUnits := Value;
   GScriptEngine.units := Value;
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

procedure TRpgDatabase.addAnim(value: TAnimTemplate);
begin
   FAnims.Add(value);
end;

procedure TRpgDatabase.addAttribute;
begin
   FAttributes.add(TAttributeTemplate.Create);
end;

procedure TRpgDatabase.addClass;
begin
   FClass.Add(TClassTemplate.Create);
end;

procedure TRpgDatabase.addClass(value: TClassTemplate);
begin
   FClass.Add(value);
end;

procedure TRpgDatabase.addCondition;
begin
   FConditions.add(TConditionTemplate.Create);
end;

procedure TRpgDatabase.addHero;
begin
   FHero.Add(THeroTemplate.Create);
end;

procedure TRpgDatabase.addHero(value: THeroTemplate);
begin
   FHero.Add(value);
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

procedure TRpgDatabase.addItem(value: TItemTemplate; slot: TItemType);
begin
   FItems[slot].Add(value);
end;

procedure TRpgDatabase.addSkill;
begin
   FSkills.Add(TSkillTemplate.Create);
end;

procedure TRpgDatabase.addSkill(value: TSkillTemplate);
begin
   FSkills.Add(value);
end;

procedure TRpgDatabase.addTileset;
begin
   FTileset.Add(TTileset.Create);
end;

procedure TRpgDatabase.AfterConstruction;
begin
   inherited;
{   turbu_skills.setDatabase(self);
   turbu_characters.SetDatabase(self);
   turbu_tilesets.setDatabase(self);}
end;

function TRpgDatabase.countItems: cardinal;
var i: TItemType;
begin
   result := 0;
   for i := low(TItemType) to high(TItemType) do
      inc(result, FItems[i].High);
end;

procedure TRpgDatabase.parseMeta;
var
   newDecl: TRpgDecl;
begin
   for newdecl in GScriptEngine.decl do
      FScripts.add(TScriptRecord.create(newdecl, GScriptEngine.getExecMethod(newdecl.name)));
end;

procedure TRpgDatabase.prepareBlanks;
var
   item: TItemType;
begin
   FSerializer := TDatasetSerializer.Create;
   FClass := TCharClassList.Create;
   addClass;
   FHero := THeroTemplateList.Create;
   addHero;
   for item := low(TItemType) to high(TItemType) do
      self.addItem(item);
   FSkills := TSkillList.Create;
   addSkill;
   FAnims := TAnimList.Create;
   addAnim;
   addAttribute;
   addCondition;
   FCommand := TBattleCommandList.Create;
   FTileGroup := TTileDictionary.Create([doOwnsValues]);
   FTileset := TTilesetList.Create;
   addTileset;
   FLegacyCruft := TLegacySections.Create;
   FScripts := TScriptList.Create;
   FGlobalEvents := TMapObjectList.Create;
   FSysVocab := TStringList.Create;
   FCustomVocab := TStringList.Create;
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
