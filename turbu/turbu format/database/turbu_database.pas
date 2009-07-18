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
   classes, Generics.Collections,
//   events,
   dm_database, turbu_database_interface,
   turbu_characters, turbu_items, turbu_db_var_arrays, turbu_skills, turbu_classes,
   turbu_battle_engine, turbu_sprites, turbu_animations, turbu_resists,
   turbu_unit_dictionary, turbu_containers, turbu_script_interface, turbu_game_data,
   turbu_map_metadata, turbu_tilesets;

type
   TCharClassArray = array of TClassTemplate;
   THeroTemplateArray = array of THeroTemplate;
   TBattleEngineArray = array of TBattleEngineData;
   TMoveMatrixArray = array of TMoveMatrix;
   TItemList = TRpgObjectList<TItemTemplate>;
   TSkillGainList = TRpgObjectList<TSkillGainRecord>;
   TItemMatrix = array[TItemType] of TItemList;
   TSkillArray = array of TSkillTemplate;
   TAnimArray = array of TAnimTemplate;
   TAttribList = TRpgObjectList<TAttributeTemplate>;
   TConditionList = TRpgObjectList<TConditionTemplate>;
   TTilesetDictionary = TObjectDictionary<string, TTileset>;
   TTileDictionary = TObjectDictionary<string, TTileGroup>;

   TRpgDataTypes = (rd_class, rd_hero, rd_command, rd_item, rd_skill, rd_anim,
                    rd_attrib, rd_condition, rd_switch, rd_int, rd_float,
                    rd_string, rd_script);
   TRpgDataTypeSet = set of TRpgDataTypes;

   TBattleCommandList = class(TRpgObjectList<TBattleCommand>)
   public
      function indexOf (name: string): integer;
   end;

   TRpgDatabase = class;

   I2kDatabase = interface(IRpgDatabase)
   ['{756ECF13-2D52-4EB3-824A-CDAB7A063DBA}']
      function dbObject: TRpgDatabase;
   end;

   TRpgDatabase = class(TRpgDatafile, I2kDatabase, IRpgDatabase)
   private
      FClass: TCharClassArray;
      FHero: THeroTemplateArray;
      FSkillFuncs: TSkillGainList;
      FCommand: TBattleCommandList;
      FItems: TItemMatrix;
      FSkills: TSkillArray;
      FAnims: TAnimArray;
      FAttributes: TAttribList;
      FConditions: TConditionList;

//      FGlobalEvents: TEventBlock;
      FLayout: TGameLayout;
      FSwitches: TSwitchSection;
      FVariables: TVarSection;

      FUnits: TUnitDictionary;
      FAlgLookup: TStringList;
      FSkillAlgs: TStringList;
      FStatAlgs: TStringList;
      FSpriteList: TStringList;
      FPortraitList: TStringList;
      FAnimList: TStringList;
      FMapTree: TMapTree;

      FMoveMatrix: TMoveMatrixArray;
      FBattleStyle: TBattleEngineArray;
      FExpRecordList: TExpRecordList;
      FStatSet: TStatSet;
      FTileGroup: TTileDictionary;
      FTileset: TTilesetDictionary;

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
   protected
      class function keyChar: ansiChar; override;
   public
      constructor Create;
      constructor Load(savefile: TStream);
      destructor Destroy; override;
      procedure save(savefile: TStream); override;
      procedure copyToDB(db: TdmDatabase; typeSet: TRpgDataTypeSet = []);
      procedure copyTypeToDB(db: TdmDatabase; value: TRpgDataTypes);
      procedure loadFromDB(value: TdmDatabase);

      procedure AfterConstruction; override;

      function addSkillFunc(data: TSkillGainRecord): integer;
      function skillFuncIndex(name: string): integer; inline;

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

      function scriptBuild: boolean;

      function dbObject: TRpgDatabase;

      property skillFunc: TSkillGainList read FSkillFuncs;
      property charClasses: integer read getClassCount write setClassCount;
      property charClass: TCharClassArray read FClass write FClass;
      property heroes: integer read getHeroCount write setHeroCount;
      property hero: THeroTemplateArray read FHero write FHero;
      property skill: TSkillArray read FSkills write FSkills;
      property item: TItemMatrix read FItems write FItems;
      property items: integer read sumItems;
      property anim: TAnimArray read FAnims write FAnims;
      property attributes: TAttribList read FAttributes write FAttributes;
      property conditions: TConditionList read FConditions write FConditions;
      property command: TBattleCommandList read FCommand write FCommand;
      property commands: integer read getCommandCount write setCommandCount;
      property tileset: TTilesetDictionary read FTileset write FTileset;
//      property globalEventBlock: TEventBlock read FGlobalEvents;
      property layout: TGameLayout read FLayout write FLayout;
      property variable: TVarSection read FVariables write FVariables;
      property switch: TSwitchSection read FSwitches write FSwitches;
      property battleStyle: TBattleEngineArray read FBattleStyle write FBattleStyle;
      property moveMatrix: TMoveMatrixArray read FMoveMatrix write FMoveMatrix;
      property mapTree: TMapTree read FMapTree write FMapTree;
      property statSet: TStatSet read FStatSet write FStatSet;
      property tileGroup: TTileDictionary read FTileGroup write FTileGroup;

      property spriteList: TStringList read FSpriteList write FSpriteList;
      property portraitList: TStringList read FPortraitList write FPortraitList;
      property animList: TStringList read FAnimList write FAnimlist;
      property expRecs: TExpRecordList read FExpRecordList;
      property units: TUnitDictionary read FUnits write setUnits;
      property algorithmLookup: TStringList read FAlgLookup write FAlgLookup;
      property projectName: string read getProjectName;
   end;

var
   GDatabase: TRpgDatabase;
   GScriptEngine: IScriptEngine;

implementation
uses
   sysUtils, zlib, math, DB, TypInfo,
   {$IFDEF EDITOR}design_script_engine,{$ENDIF} archiveInterface, commons,
   turbu_constants, turbu_engines, turbu_versioning, turbu_plugin_interface,
   turbu_decl_utils, turbu_functional;

const
   DBVERSION = 21;

{ TRpgDatabase }

constructor TRpgDatabase.Create;
var
   i: TItemType;
begin
   inherited Create;
   FName := 'TURBU RPG Database';
   FID := DBVERSION;
   FUnits := TUnitDictionary.Create([doOwnsValues], 20);
   FStatAlgs := TStringList.Create;
   FSkillAlgs := TStringList.Create;
   for I := low(TItemType) to high(TItemType) do
      FItems[i] := TItemList.Create;
   FAttributes := TAttribList.Create;
   FConditions := TConditionList.Create;
   FSkillFuncs := TSkillGainList.Create;
   FLayout := TGameLayout.Create;
   self.prepareBlanks;
end;

function TRpgDatabase.dbObject: TRpgDatabase;
begin
   assert(assigned(self));
   result := self;
end;

constructor TRpgDatabase.Load(savefile: TStream);
type
   TItemClass = class of TItemTemplate;
   TSkillClass = class of TSkillTemplate;
var
   substream: TStream;
   i, j, k: integer;
   item: TItemType;
   items: array[TItemType] of TItemClass;
   skills: TSkillClass;
   dummy: string;
   filename, uFilename: string;

   procedure loadStringList(stream: TStream; var list: TStringList);
   var
      newstream: TStringStream;
      data: string;
   begin
      k := stream.readInt;
      data := stream.readString;
      newStream := TStringStream.Create(data);
      try
         newStream.Position := 0;
         list := TStringList.Create;
         list.LoadFromStream(newStream);
         lassert(list.Count = k);
      finally
         newStream.Free;
      end;
   end;

begin
   skills := nil;
   items[it_junk] := TJunkTemplate;
   items[it_weapon] := TWeaponTemplate;
   items[it_armor] := TArmorTemplate;
   items[it_medicine] := TMedicineTemplate;
   items[it_upgrade] := TStatItemTemplate;
   items[it_book] := TSkillBookTemplate;
   items[it_skill] := TSkillItemTemplate;
   items[it_variable] := TVariableItemTemplate;
   items[it_script] := TScriptItemTemplate;
   for item := low(TItemType) to high(TItemType) do
      FItems[item] := TRpgObjectList<TItemTemplate>.Create;
   FAttributes := TAttribList.Create;
   FConditions := TConditionList.Create;
   FSkillFuncs := TSkillGainList.Create;
   self.prepareBlanks;
   GDatabase := self;

   inherited load(savefile);
   lassert(FName = 'TURBU RPG Database');
   lassert(FID = DBVERSION);

   substream := GArchives[DATABASE_ARCHIVE].getFile('mapTree.tdf');
   try
      FMapTree := TMapTree.Load(subStream);
      lassert(savefile.readChar = 'T');
      lassert(FMapTree.Count = savefile.readInt);
      lassert(savefile.readChar = 't');
   finally
      substream.free;
   end;

   lassert(savefile.readChar = 'S');
   FStatSet.free;
   FStatSet := TStatSet.load(savefile);
   lassert(savefile.readChar = 's');

   substream := GArchives[DATABASE_ARCHIVE].getFile('lists.tdf');
   try
      lassert(subStream.readChar = 'L');
      loadStringList(subStream, FSkillAlgs);
      loadStringList(subStream, FStatAlgs);
      loadStringList(subStream, FSpriteList);
      loadStringList(subStream, FPortraitList);
      loadStringList(subStream, FAnimList);
      lassert(subStream.readChar = 'l');
   finally
      substream.free;
   end;
   lassert(savefile.readInt = FSkillAlgs.Count);
   lassert(savefile.readInt = FStatAlgs.Count);
   lassert(savefile.readInt = FSpriteList.Count);
   lassert(savefile.readInt = FPortraitList.Count);
   lassert(savefile.readInt = FAnimList.Count);

   substream := GArchives[DATABASE_ARCHIVE].getFile('metadata');
   try
      FAlgLookup.LoadFromStream(substream);
   finally
      substream.Free;
   end;

   k := savefile.readInt;
   FUnits := TUnitDictionary.Create([doOwnsValues], k * 2);
   for filename in GArchives[DATABASE_ARCHIVE].allFiles('scripts') do
   begin
      substream := GArchives[DATABASE_ARCHIVE].getFile(filename);
      try
         uFilename := upperCase(ExtractFileName(filename));
         FUnits.Add(uFileName, TStringList.Create);
         FUnits[uFileName].LoadFromStream(substream);
      finally
         substream.Free;
      end;
   end;
   lassert(k = FUnits.Count);
   {$IFDEF EDITOR}
   GDScriptEngine.units := FUnits;
   {$ENDIF}
   self.scriptBuild;
   self.parseMeta;

   turbu_characters.setDatabase(self);
   turbu_skills.setDatabase(self);
//   turbu_tilesets.setDatabase(self);

   setLength(FClass, savefile.readInt + 1);
   if high(FClass) > 0 then
   begin
      substream := GArchives[DATABASE_ARCHIVE].getFile('classes.tdf');
      try
         lassert(subStream.readChar = 'C');
         lassert(subStream.readInt = high(FClass));
         for I := 1 to high(FClass) do
            FClass[i] := TClassTemplate.Load(subStream);
         lassert(subStream.readChar = 'c');
      finally
         substream.Free;
      end;
   end;

   setLength(FHero, savefile.readInt + 1);
   if high(FHero) > 0 then
   begin
      substream := GArchives[DATABASE_ARCHIVE].getFile('heroes.tdf');
      try
         lassert(subStream.readChar = 'H');
         lassert(subStream.readInt = high(FHero));
         for I := 1 to high(FHero) do
            FHero[i] := THeroTemplate.Load(subStream);
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

   setLength(FSkills, savefile.readInt + 1);
   if high(FSkills) > 0 then
   begin
      substream := GArchives[DATABASE_ARCHIVE].getFile('skills.tdf');
      try
         lassert(subStream.readChar = 'S');
         lassert(subStream.readInt = high(FSkills));
         for I := 1 to high(FSkills) do
         begin
            case subStream.readChar of
               'N': skills := TNormalSkillTemplate;
               'V': skills := TVariableSkillTemplate;
               'T': skills := TTeleportSkillTemplate;
               else lassert(false);
            end;
            FSkills[i] := skills.Load(subStream);
         end;
         lassert(subStream.readChar = 's');
      finally
         substream.Free;
      end;
   end;

   setLength(FAnims, savefile.readInt + 1);
   if high(FAnims) > 0 then
   begin
      substream := GArchives[DATABASE_ARCHIVE].getFile('animations.tdf');
      try
         lassert(subStream.readChar = 'A');
         lassert(subStream.readInt = high(FAnims));
         for I := 1 to high(FAnims) do
            FAnims[i] := TAnimTemplate.Load(subStream);
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
   begin
      dummy := savefile.readString;
      FTileSet.Add(dummy, TTileSet.Load(savefile));
   end;
//   savefile.readDict<TTileset>(FTileset);
   lassert(savefile.readChar = 't');

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

   lassert(savefile.readChar = 'B');
   setLength(FBattleStyle, savefile.readByte);
   for I := 0 to high(FBattleStyle) do
   begin
      dummy := savefile.readString; //order of operations issue
      FBattleStyle[i] := TBattleEngineData(requireEngine(et_battle, dummy, TVersion.Create(savefile.readInt)));
   end;
   lassert(savefile.readChar = 'b');

   lassert(savefile.readChar = 'L');
   FLayout := TGameLayout.Load(savefile);
   lassert(savefile.readChar = 'l');

   lassert(savefile.readChar = 'd');
end;

procedure TRpgDatabase.save(savefile: TStream);
var
   substream: TMemoryStream;
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

begin
   inherited save(savefile);

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

   savefile.writeChar('S');
   FStatSet.save(savefile);
   savefile.writeChar('s');

   savefile.writeInt(FSkillAlgs.Count);
   savefile.writeInt(FStatAlgs.Count);
   savefile.writeInt(FSpriteList.Count);
   savefile.writeInt(FPortraitList.Count);
   savefile.writeInt(FAnimList.Count);
   substream := TMemoryStream.Create;
   try
      substream.writeChar('L');
      saveStringList(substream, FSkillAlgs);
      saveStringList(substream, FStatAlgs);
      saveStringList(substream, FSpriteList);
      saveStringList(substream, FPortraitList);
      saveStringList(substream, FAnimList);
      substream.writeChar('l');
      GArchives[DATABASE_ARCHIVE].writeFile('lists.tdf', substream);
   finally
      substream.free;
   end;

   substream := TMemoryStream.Create;
   try
      FAlgLookup.SaveToStream(substream);
      GArchives[DATABASE_ARCHIVE].writeFile('metadata', substream);
   finally
      substream.Free;
   end;

   savefile.writeInt(FUnits.Count);
   for filename in FUnits.Keys do
   begin
      subStream := TMemoryStream.Create;
      try
         FUnits[filename].SaveToStream(substream);
         GArchives[DATABASE_ARCHIVE].writeFile('scripts\' + filename, substream);
      finally
         substream.Free;
      end;
   end;

   savefile.writeInt(high(FClass));
   if high(FClass) > 0 then
   begin
      subStream := TMemoryStream.Create;
      try
         subStream.writeChar('C');
         subStream.writeInt(high(FClass));
         for I := 1 to high(FClass) do
            FClass[i].save(substream);
         subStream.writeChar('c');
         GArchives[DATABASE_ARCHIVE].writeFile('classes.tdf', substream);
      finally
         substream.Free;
      end;
   end;

   savefile.writeInt(high(FHero));
   if high(FHero) > 0 then
   begin
      subStream := TMemoryStream.Create;
      try
         subStream.writeChar('H');
         subStream.writeInt(high(FHero));
         for I := 1 to high(FHero) do
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

   savefile.writeInt(high(FSkills));
   if high(FSkills) > 0 then
   begin
      subStream := TMemoryStream.Create;
      try
         subStream.writeChar('S');
         subStream.writeInt(high(FSkills));
         for I := 1 to high(FSkills) do
            FSkills[i].save(substream);
         subStream.writeChar('s');
         GArchives[DATABASE_ARCHIVE].writeFile('skills.tdf', substream);
      finally
         substream.Free;
      end;
   end;

   savefile.writeInt(high(FAnims));
   if high(FAnims) > 0 then
   begin
      subStream := TMemoryStream.Create;
      try
         subStream.writeChar('A');
         subStream.writeInt(high(FAnims));
         for I := 1 to high(FAnims) do
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

   savefile.writeChar('G');
   saveTileGroups(savefile);
//   savefile.writeDict<TTileGroup>(FTileGroup);
   savefile.writeChar('g');

   savefile.writeChar('T');
   saveTilesets(savefile);
//   savefile.writeDict<TTileset>(FTileset);
   savefile.writeChar('t');

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

   savefile.writeChar('B');
   savefile.WriteByte(length(FBattleStyle));
   for I := 0 to high(FBattleStyle) do
   begin
      savefile.writeString(FBattleStyle[i].name);
      savefile.writeInt(FBattleStyle[i].version.value);
   end;
   savefile.writeChar('b');

   savefile.writeChar('L');
   FLayout.save(savefile);
   savefile.writeChar('l');

   savefile.writeChar('d');
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
   enumerator: TPair<string, TTileSet>;
begin
   savefile.writeInt(FTileSet.Count);
   for enumerator in FTileSet do
   begin
      savefile.writeString(enumerator.Key);
      enumerator.Value.save(savefile);
   end;
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
         {$IFDEF EDITOR}
         GDScriptEngine.script := ansiString(format(SCRIPT, [list.CommaText]));
         {$ENDIF}
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

destructor TRpgDatabase.Destroy;
var
   i, j: integer;
begin
   FExpRecordList.Free;
   FUnits.Free;
   FStatAlgs.Free;
   FSpriteList.Free;
   FPortraitList.Free;
   FAnimList.Free;
   for i := 0 to high(FClass) do
      FClass[i].Free;
   for i := 0 to high(FHero) do
      FHero[i].Free;
   FCommand.Free;
   for i := 0 to high(FSkills) do
      FSkills[i].Free;
   for i := FSkillFuncs.high downto 0 do
   begin
      j := FSkillAlgs.IndexOfObject(FSkillFuncs[i]);
      if j <> -1  then
         FSkillAlgs.Delete(j); //this should stop the strange memory leak issues
//      FSkillFuncs[i].Free;
   end;
   FSkillFuncs.Free;
   if assigned(FSkillAlgs) then //gotta do it this way in case an exception was
   begin                        //raised in the constructor
      for i := 0 to FSkillAlgs.Count - 1 do
         FSkillAlgs.Objects[i].Free;
      FSkillAlgs.Destroy;
   end;
   for i := 0 to ord(high(TItemType)) do
      FItems[TItemType(i)].free;
   for i := 0 to high(FAnims) do
      FAnims[i].Free;
   FConditions.Free;
   FAttributes.Free;
   FAlgLookup.Free;
   FLayout.Free;
   FMapTree.Free;
   FStatSet.Free;
   FTileGroup.Free;
   FTileset.Free;
   inherited Destroy;
end;

procedure TRpgDatabase.copyToDB(db: TdmDatabase; typeSet: TRpgDataTypeSet = []);
var
   i: TRpgDataTypes;
begin
   db.beginUpload;
   if typeSet = [] then
      typeSet := [low(TRpgDataTypes)..high(TRpgDataTypes)];
   for i in typeSet do
      copyTypeToDB(db, i);
   db.endUpload;
end;

procedure TRpgDatabase.copyTypeToDB(db: TdmDatabase; value: TRpgDataTypes);
var
   i: integer;
   iterator: TRpgDatafile;
   dummy: TDataSet;
begin
   case value of
      rd_class:
      begin
         for I := 1 to high(FClass) do
            FClass[i].upload(db.charClasses);
         db.charClasses.postSafe;
      end;
      rd_hero: ;
      rd_command:
      begin
         for iterator in FCommand do
            if iterator.id > 0 then
               iterator.upload(db.commands);
         db.commands.postSafe;
      end;
      rd_item:
      begin
         for i := ord(low(TItemType)) to ord(high(TItemType)) do
         begin
            dummy := db.FindComponent('items') as TDataset;
//            dummy := db.FindComponent(stringReplace(GetEnumName(TypeInfo(TItemType), i), 'it_', 'items_', [])) as TDataset;
            for iterator in FItems[TItemType(i)] do
               if iterator.id > 0 then
               begin
                  iterator.upload(dummy);
                  dummy.FieldByName('itemType').AsInteger := i;
               end;
            dummy.postSafe;
         end;
      end;
      rd_skill:
      begin
         for I := 1 to high(FSkills) do
            FSkills[i].upload(db.skills);
         db.skills.postSafe;
      end;
      rd_anim:
      begin
         for I := 1 to high(FAnims) do
            FAnims[i].upload(db.animations);
         db.animations.postSafe;
      end;
      rd_attrib:
      begin
         for I := 1 to FAttributes.High do
            FAttributes[i].upload(db.attributes);
         db.attributes.postSafe;
      end;
      rd_condition:
      begin
         for I := 1 to FConditions.High do
            FConditions[i].upload(db.conditions);
         db.conditions.postSafe;
      end;
      rd_switch: ;
      rd_int: ;
      rd_float: ;
      rd_string: ;
      rd_script:
      begin
         {$IFDEF EDITOR}
         GDScriptEngine.upload(db);
         {$ENDIF}
         for i := 0 to FSkillFuncs.High do
            FSkillFuncs[i].upload(db.SkillGainRecords);
         for i := 0 to FExpRecordList.High do
            FExpRecordList[i].upload(db.expCalcRecords);
      end;
      else assert(false);
   end;
   for dummy in db.datasets do
      dummy.First;
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
   result := high(FClass);
end;

procedure TRpgDatabase.setClassCount(const value: integer);
var
   i: word;
begin
   i := self.charClasses + 1;
   if value > i then
   begin
      setLength(FClass, value + 1);
      for I := i + 1 to value do
         FClass[i] := TClassTemplate.Create;
   end
   else if value < i then
   begin
      for I := i downto value + 1 do
         FClass[i].Free;
      setLength(FClass, value + 1);
   end;
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
   result := high(FHero);
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
   i := self.heroes + 1;
   if value > i then
   begin
      setLength(FHero, value + 1);
      for I := i + 1 to value do
         FHero[i] := THeroTemplate.Create;
   end
   else if value < i then
   begin
      for I := i downto value + 1 do
         FHero[i].Free;
      setLength(FHero, value + 1);
   end;
end;

procedure TRpgDatabase.setUnits(const Value: TUnitDictionary);
begin
   FUnits := Value;
   {$IFDEF EDITOR}
   GDScriptEngine.units := Value;
   {$ENDIF}
end;

function TRpgDatabase.skillFuncIndex(name: string): integer;
begin
   result := FSkillAlgs.IndexOf(name)
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
   setLength(FAnims, length(FAnims) + 1);
   FAnims[high(FAnims)] := TAnimTemplate.Create;
end;

procedure TRpgDatabase.addAnim(value: TAnimTemplate);
begin
   setLength(FAnims, length(FAnims) + 1);
   FAnims[high(FAnims)] := value;
end;

procedure TRpgDatabase.addAttribute;
begin
   FAttributes.add(TAttributeTemplate.Create);
end;

procedure TRpgDatabase.addClass;
begin
   setLength(FClass, length(FClass) + 1);
   FClass[high(FClass)] := TClassTemplate.Create;
end;

procedure TRpgDatabase.addClass(value: TClassTemplate);
begin
   setLength(FClass, length(FClass) + 1);
   FClass[high(FClass)] := value;
end;

procedure TRpgDatabase.addCondition;
begin
   FConditions.add(TConditionTemplate.Create);
end;

procedure TRpgDatabase.addHero;
begin
   setLength(FHero, length(FHero) + 1);
   FHero[high(FHero)] := THeroTemplate.Create;
end;

procedure TRpgDatabase.addHero(value: THeroTemplate);
begin
   setLength(FHero, length(FHero) + 1);
   FHero[high(FHero)] := value;
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
   setLength(FSkills, length(FSkills) + 1);
   FSkills[high(FSkills)] := TSkillTemplate.Create;
end;

procedure TRpgDatabase.addSkill(value: TSkillTemplate);
begin
   setLength(FSkills, length(FSkills) + 1);
   FSkills[high(FSkills)] := value;
end;

function TRpgDatabase.addSkillFunc(data: TSkillGainRecord): integer;
begin
   result := FSkillFuncs.indexOf(data);
   if result = -1 then
   begin
      result := FSkillFuncs.Count;
      FSkillFuncs.Add(data);
      FSkillAlgs.AddObject(data.name, data);
   end
   else data.free;
end;

procedure TRpgDatabase.AfterConstruction;
begin
   inherited;
   turbu_skills.setDatabase(self);
   turbu_characters.SetDatabase(self);
//   turbu_tilesets.setDatabase(self);
end;

function TRpgDatabase.countItems: cardinal;
var i: TItemType;
begin
   result := 0;
   for i := low(TItemType) to high(TItemType) do
      inc(result, FItems[i].High);
end;

procedure TRpgDatabase.parseMeta;
{$IFDEF EDITOR}
var
   dummy: string;
   newDecl: TRpgDecl;
{$ENDIF}
begin
   {$IFDEF EDITOR}
   for newdecl in GDScriptEngine.decl do
   begin
      dummy := FAlgLookup.Values[newDecl.name];
      case signatureMatch(newDecl) of
         ssExpCalc: expRecs.Add(TExpCalcRecord.Create(newDecl.name, dummy));
         ssSkillCheck: addSkillFunc(TSkillGainRecord.Create(newDecl.name, dummy, sf_bool, false));
      end;
   end;
   {$ENDIF}
end;

procedure TRpgDatabase.prepareBlanks;
var
   item: TItemType;
begin
   addClass;
   addHero;
   for item := low(TItemType) to high(TItemType) do
      self.addItem(item);
   addSkill;
   addAnim;
   addAttribute;
   addCondition;
   FExpRecordList := TExpRecordList.Create;
   FCommand := TBattleCommandList.Create;
   FAlgLookup := TStringList.Create;
   FTileGroup := TTileDictionary.Create([doOwnsValues]);
   FTileset := TTilesetDictionary.Create([doOwnsValues]);
end;

{ TBattleCommandList }

function TBattleCommandList.indexOf(name: string): integer;
var
   iterator: TBattleCommand;
begin
   result := -1;
   for iterator in self do
      if iterator.name = name then
         Exit(iterator.id);
end;

initialization
begin
   GDatabase := nil;
end;

finalization
begin
   GDatabase.Free;
end;

end.
