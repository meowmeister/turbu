unit rm2_turbu_database;
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

interface

uses
   classes,
   LDB, LMT, turbu_database, events, turbu_unit_dictionary, conversion_report;

type

   T2k2Database = class helper for TRpgDatabase
   private
      procedure buildNameLists(base: TLcfDatabase; ConversionReport: IConversionReport;
                          spriteList, portList, animList, battleSpriteList: TStringList);
      procedure convertEvents(block: TEventBlock);
      procedure SaveScript(const script: utf8String);
   public
      constructor convert(base: TLcfDatabase; tree: TFullTree; dic: TUnitDictionary;
                          legacy: TLegacySections; ConversionReport: IConversionReport;
                          spriteList, portList, animList, battleSpriteList: TStringList);
   end;

var
   GLcfDatabase: TLcfDatabase;

implementation
uses
   sysUtils, StrUtils,
   turbu_characters, turbu_items, turbu_skills, turbu_animations, conversion_table,
   turbu_resists, turbu_map_metadata,
   rm2_turbu_items, rm2_turbu_characters, rm2_turbu_skills, rm2_turbu_animations,
   rm2_turbu_resists, rm2_turbu_map_metadata, rm2_turbu_tilesets, turbu_sprites,
   turbu_versioning, turbu_plugin_interface, turbu_constants, turbu_sdl_image,
   turbu_tbi_lib, turbu_tilesets, rm2_turbu_map_objects, turbu_map_objects,
   hero_data, locate_files, turbu_maps,
   archiveInterface, formats, commons, turbu_battle_engine, turbu_engines, logs,
   turbu_map_engine, EB_RpgScript,
   SDL, SDL_13, sdl_image, sg_defs;

const
   MOVE_MATRIX: array[0..3, 0..3] of byte = ((0, 1, 2, 1), (3, 4, 5, 4), (6, 7, 8, 7), (9, 10, 11, 10));

{ T2k2Database }

procedure T2k2Database.buildNameLists(base: TLcfDatabase; ConversionReport: IConversionReport;
                          spriteList, portList, animList, battleSpriteList: TStringList);
var
   i: integer;
begin
   for i := 1 to base.heroes do
      spriteList.add(string(base.hero[i].sprite));

   for i := 1 to base.heroes do
      portList.Add(string(base.hero[i].portrait));

   for i := 1 to base.anims do
      animList.Add(string(base.anim[i].filename));

   { worry about battlesprites later }
end;

function setup2kCommand(value: integer): TBattleCommand;
const
   table: array [1..4] of TBattleVocabSet = (bv_attack, bv_skill, bv_defend, bv_item);
begin
   result := TBattleCommand.Create;
   result.id := value;
   result.name := string(GLcfDatabase.battleVocab[table[value]]);
   case value of
      1: result.style := cs_weapon;
      2: result.style := cs_skill;
      3: result.style := cs_defend;
      4: Result.style := cs_item;
   end;
   if value <> 4 then
      result.value := value
   else result.value := -1;
end;

constructor T2k2Database.convert(base: TLcfDatabase; tree: TFullTree;
   dic: TUnitDictionary; legacy: TLegacySections; ConversionReport: IConversionReport;
   spriteList, portList, animList, battleSpriteList: TStringList);
var
   i, j: integer;
   counter, classes: integer;
   classTable, heroClassTable: TConversionTable;
   battleEngine: TBattleEngineData;
   defMoveMatrix: TMoveMatrix;
   moveArray: ^TMoveMatrixArray;
begin
   // setup
   self.Create;
   GDatabase := self;
   GLcfDatabase := base;
   self.units.free;
   self.units := dic;
   FLegacyCruft.Free;
   FLegacyCruft := legacy;
   self.ConvertEvents(GlobalEventBlock as TEventBlock);
   self.scriptBuild;
   self.parseMeta;
   assert(SDL_WasInit(SDL_INIT_VIDEO) = SDL_INIT_VIDEO);

   // make sure required battle engine is available from plugins
   case GProjectFormat of
      pf_2k: battleEngine := requireEngine(et_battle, 'First-person battle engine', TVersion.Create(0, 1, 1)) as TBattleEngineData;
      pf_2k3: battleEngine := requireEngine(et_battle, 'Active-time battle engine', TVersion.Create(0, 1, 1)) as TBattleEngineData;
   else
      begin
         battleEngine := nil;
         assert(false);
      end;
   end;
   self.battleStyle.add(battleEngine);
   self.mapEngines.add(requireEngine(et_map, 'TURBU basic map engine', TVersion.Create(0, 1, 0)) as TMapEngineData);

   // define default move matrix
   setLength(defMoveMatrix, length(MOVE_MATRIX));
   for i := 0 to high(MOVE_MATRIX) do
   begin
      setLength(defMoveMatrix[i], length(MOVE_MATRIX[i]));
      for j := 0 to high(MOVE_MATRIX[i]) do
         defMoveMatrix[i, j] := MOVE_MATRIX[i, j];
   end;
   moveArray := @self.moveMatrix;
   setLength(moveArray^, length(moveArray^) + 1);
   self.moveMatrix[high(self.moveMatrix)] := defMoveMatrix;

   // create conversion tables
   classTable := TConversionTable.Create;
   heroClassTable := TConversionTable.Create;
   self.statSet := TStatSet.Create;

   try
      buildNameLists(base, ConversionReport, spriteList, portList, animList, battleSpriteList);

      ConversionReport.newStep('Converting heroes');
      // COMMANDS
      self.command.add(TBattleCommand.Create);
      if GProjectFormat = pf_2k3 then
      begin
         for i := 1 to base.commands do
            command.add(TBattleCommand.convert(base.command[i], i));
      end else
      begin
         for i := 1 to 4 do
            command.add(setup2kCommand(i));
         for i := 1 to base.heroes do
            if base.hero[i].hasSkillName then
            begin
               command.add(TBattleCommand.Create);
               command.Last.id := command.High;
               command.Last.name := (string(base.hero[i].skillName));
               command.Last.style := cs_skillgroup;
               command.Last.value := command. High;
            end;
         // end FOR
      end;

      // CLASS RECORDS
      if GProjectFormat = pf_2k3 then
      begin
         self.charClasses := base.charClasses;
         counter := 0;
         for i := 1 to base.charClasses do
         begin
            if not isEmpty(base.charClass[i]) then
            begin
               charClass[i] := TClassTemplate.convert(base.charClass[i], self.statSet);
               classTable.add(i, i - counter);
            end
            else
               inc(counter);
         end;
         counter := base.charClasses;
         classes := classTable.count;
      end
      else begin
         counter := 0;
         classes := 0;
      end;
      for i := 1 to base.heroes do
      begin
         if (not isEmpty(base.hero[i])) and (base.hero[i].classNum = 0) then
         begin
            GDatabase.addClass(TClassTemplate.convert(base.hero[i], base, self.statSet));
            inc(counter);
            heroClassTable.add(i, counter);
         end;
      end;

      // HERO RECORDS
      for i := 1 to base.heroes do
         if base.hero[i].classNum <> 0 then
            GDatabase.addHero(THeroTemplate.convert(base.hero[i], classTable, base, self.statSet))
         else GDatabase.addHero(THeroTemplate.convert(base.hero[i], heroClassTable, base, self.statSet));

      ConversionReport.newStep('Converting Items');
      // ITEMS
      for i := 1 to base.items do
         TItemTemplate.addNewItem(base.item[i]);

      // LOAD ITEMS INTO CLASS/HERO RECORDS
      j := classes;
      for i := j + 1 to self.charClasses do
         if (not isEmpty(base.hero[i - classes])) and (base.hero[i - classes].classNum = 0) then
         begin
            inc(j);
            self.charClass[j].loadEq(base.hero[i - classes]);
         end;

      for i := 1 to base.heroes do
         self.hero[i].loadEq(base.hero[i]);

      ConversionReport.newStep('Converting Skills');
      // SKILLS
      for i := 1 to base.skills do
         TSkillTemplate.addNewSkill(base.skill[i]);

      ConversionReport.newStep('Converting Animations');
      // ANIMATIONS
      for i := 1 to base.anims do
         self.addAnim(TAnimTemplate.convert(base.anim[i], i));

      ConversionReport.newStep('Converting Attributes and Conditions');
      // ATTRIBUTES
      for i := 1 to base.attributes do
         self.attributes.add(TAttributeTemplate.convert(base.attribute[i], i));

      // CONDITIONS
      for i := 1 to base.conditions do
         self.conditions.add(TConditionTemplate.convert(base.condition[i], i));

      ConversionReport.newStep('Converting Tilesets');
      //TILESETS
      for I := 1 to base.getMaxChipsets do
         if not base.getChipset(i).empty then
            self.tileset.Add(TTileset.Convert(base.getChipset(i), i));

      ConversionReport.newStep('Preparing layout');
      self.layout.width := LOGICAL_SIZE.X;
      self.layout.height := LOGICAL_SIZE.Y;
      self.layout.physWidth := PHYSICAL_SIZE.X;
      self.layout.physHeight := PHYSICAL_SIZE.Y;

      ConversionReport.newStep('Converting map tree');
      self.mapTree := TMapTree.convert(tree, true);

      for I := 1 to base.variables.len do
         self.variable.Add(string(base.variables.name[i]));
      assert(self.variable.Count = base.variables.len);

      for I := 1 to base.switches.len do
         self.switch.Add(string(base.switches.name[i]));
      assert(self.switch.Count = base.switches.len);
   finally
      classTable.free;
      heroClassTable.free;
   end;
end;

procedure T2k2Database.convertEvents(block: TEventBlock);
var
   nameList: TStringList;
   i: integer;
   obj: TRpgMapObject;
   scriptName: string;
begin
   FGlobalScriptBlock := TEBUnit.Create(nil);
   nameList := TStringList.Create;
   try
      nameList.Sorted := true;
      FGlobalScriptBlock.name := 'GlobalEvents';
      for I := 0 to block.len - 1  do
      begin
         obj := TRpgMapObject.Convert(block.events[i], nameList,
            procedure(script: TEBProcedure)
            begin
               FGlobalScriptBlock.add(script);
               script.Name := copy(script.name, 1, length(script.name) - 6)
            end);
         scriptName := obj.pages[0].scriptName;
         if AnsiEndsText('_page1', scriptName) then
            obj.pages[0].scriptName := copy(scriptName, 1, length(scriptName) - 6);
         GlobalEvents.Add(obj);
      end;
      self.saveScript(utf8String(FGlobalScriptBlock.serialize));
   finally
      nameList.Free;
   end;
end;

procedure T2k2Database.SaveScript(const script: utf8String);
var
   stream: TMemoryStream;
   filename: TFilenameData;
begin
   self.scriptFormat := sfEvents;
   stream := TMemoryStream.Create;
   try
      stream.WriteBuffer(script[1], length(script));
      filename := GArchives[SCRIPT_ARCHIVE].MakeValidFilename('globalevents.trs');
      GArchives[SCRIPT_ARCHIVE].writeFile(filename.name, stream);
      self.scriptFile := filename.name;
   finally
      stream.Free;
   end;
end;

end.
