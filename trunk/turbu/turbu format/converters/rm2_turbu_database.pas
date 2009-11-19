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
   LDB, LMT, turbu_database, turbu_unit_dictionary, conversion_report;

type

   T2k2Database = class helper for TRpgDatabase
   private
      procedure buildNameLists(base: TLcfDatabase; ConversionReport: IConversionReport);
   public
      constructor convert(base: TLcfDatabase; tree: TFullTree; dic: TUnitDictionary;
                          legacy: TLegacySections; ConversionReport: IConversionReport);
      function lookupMapSprite(name: ansiString; index: byte): integer;
      function lookupPortrait(name: ansiString; index: byte): integer;
   end;

var
   GLcfDatabase: TLcfDatabase;

implementation
uses
   sysUtils, classes, zlib,
   turbu_characters, turbu_items, turbu_skills, turbu_animations, conversion_table,
   turbu_resists, turbu_map_metadata,
   rm2_turbu_items, rm2_turbu_characters, rm2_turbu_skills, rm2_turbu_animations,
   rm2_turbu_resists, rm2_turbu_map_metadata, rm2_turbu_tilesets, turbu_sprites,
   turbu_versioning, turbu_plugin_interface, turbu_constants, turbu_sdl_image,
   turbu_tbi_lib, turbu_tilesets,
   hero_data, locate_files,
   archiveInterface, formats, commons, turbu_battle_engine, turbu_engines, logs,
   turbu_map_engine,
   SDL, SDL_13, sdl_image;

var
   nameTable: TNameTable;

const
   INDEX_SPRITE = 0;
   INDEX_PORTRAIT = 1;
   INDEX_ANIM = 2;
   MOVE_MATRIX: array[0..3, 0..3] of byte = ((0, 1, 2, 1), (3, 4, 5, 4), (6, 7, 8, 7), (9, 10, 11, 10));

function convertSprite(name: string; id: integer): boolean; forward;
function convertPortrait(name: string): boolean; forward;
function convertAnim(name: string): boolean; forward;

{ T2k2Database }

procedure T2k2Database.buildNameLists(base: TLcfDatabase; ConversionReport: IConversionReport);
var
   counter: integer;
   dummy: string;
   i, j: integer;
   // converter: TMemoryStream;

   function scanMapSprite(name: ansiString; index: byte): boolean;
   begin
      result := true;
      dummy := 'mapsprite\' + unicodeString(name) + ' ' + intToStr(index);
      if (name <> '') and (nameTable.indexOf(dummy, INDEX_SPRITE) = -1) then
      begin
         nameTable.add(dummy, counter);
         result := convertSprite(unicodeString(name), index);
         inc(counter);
      end;
   end;

   function scanPortrait(name: ansiString): boolean;
   begin
      result := true;
      dummy := 'portrait\' + unicodeString(name);
      if (name <> '') and (nameTable.indexOf(dummy, INDEX_PORTRAIT) = -1) then
      begin
         nameTable.add(dummy, counter);
         result := convertPortrait(unicodeString(name));
         inc(counter);
      end;
   end;

   function scanAnim(name: ansiString): boolean;
   begin
      result := true;
      dummy := 'animations\' + unicodeString(name);
      if (name <> '') and (nameTable.indexOf(dummy, INDEX_ANIM) = -1) then
      begin
         nameTable.add(dummy, counter);
         result := convertAnim(unicodeString(name));
         inc(counter);
      end;
   end;

   function saveList(name: string; appendZero: boolean = false): TStringList;
   begin
      j := -1;
      result := TStringList.Create;
      while (i < nameTable.len) and (nameTable[i].id > j) do
      begin
         assert(nameTable[i].id = j + 1);
         dummy := nameTable[i].name;
         if appendZero then
            dummy := dummy + ', 0';
         result.add(dummy);
         inc(i);
         inc(j);
      end;
   end;

begin
   { Set up name table }
   ConversionReport.newStep('Preparing name tables.');
   ConversionReport.newStep('Hero sprites.');
   nameTable.newDivision;
   counter := 0;
   for i := 1 to base.heroes do
      if not scanMapSprite(base.hero[i].sprite, base.hero[i].spriteIndex) then
         base.hero[i].sprite := '';

   ConversionReport.newStep('Hero portraits.');
   nameTable.newDivision;
   counter := 0;
   for i := 1 to base.heroes do
      if not scanPortrait(base.hero[i].portrait) then
         base.hero[i].portrait := '';

   ConversionReport.newStep('Battle animations.');
   nameTable.newDivision;
   counter := 0;
   for i := 1 to base.anims do
      if not scanAnim(base.anim[i].filename) then
         base.anim[i].filename := '';

   { worry about battlesprites later }

   i := 0;

   self.spriteList := saveList('spritelist', true);
   self.portraitList := saveList('portraitlist');
   self.animlist := saveList('animlist');
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
   dic: TUnitDictionary; legacy: TLegacySections; ConversionReport: IConversionReport);
var
   i, j: integer;
   counter, classes: integer;
   classTable, heroClassTable: TConversionTable;
   reader: TStream;
   battleEngine: TBattleEngineData;
   defMoveMatrix: TMoveMatrix;
   moveArray: ^TMoveMatrixArray;
begin
   // setup
   self.Create;
   GDatabase := self;
   GLcfDatabase := base;
   reader := GArchives[0].getFile('metadata');
   try
      self.algorithmLookup.loadFromStream(reader);
   finally
      reader.free;
   end;
   self.units.free;
   self.units := dic;
   FLegacyCruft.Free;
   FLegacyCruft := legacy;
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
   nameTable.free;
   nameTable := TNameTable.Create;
   self.statSet := TStatSet.Create;

   try
      buildNameLists(base, ConversionReport);

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
               command.Last.id := command. High;
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
         classes := classTable.len;
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

function T2k2Database.lookupMapSprite(name: ansiString; index: byte): integer;
begin
   assert(assigned(nameTable));
   assert(index in [0..7]);
   result := nameTable.indexOf(format('mapsprite\%s %d', [name, index]), INDEX_SPRITE);
   if result < 0 then
      raise EConversionTableError.CreateFmt('Sprite name "%s" not found in conversion table!', [name]);
end;

function T2k2Database.lookupPortrait(name: ansiString; index: byte): integer;
begin
   assert(assigned(nameTable));
   assert(index in [0..15]);
   result := (nameTable.indexOf('portrait\' + unicodeString(name), INDEX_PORTRAIT) * 16) + index;
   if result < 0 then
      raise EConversionTableError.CreateFmt('Portrait name "%s" not found in conversion table!', [name]);
end;

{ Classless }

function convertImage(image: PSdlSurface; id: integer; frame, sprite, sheet: TPoint; name, style: string): boolean;
var
   blitSurface: PSdlSurface;
   framesPerSprite: integer;
   startingPoint: TPoint;
   i: integer;
   srcrect, dstrect: TSDLRect;
   convertedImage: TStream;
   writename: string;
begin
   framesPerSprite := sprite.X * sprite.Y;
   blitSurface := TSdlSurface.Create(frame.X, frame.Y * framesPerSprite, 8, 0, 0, 0, 0);
   try
      if not blitSurface.SetPalette(image.format.palette.colors, 0, image.format.palette.count) then
         raise EInvalidImage.CreateFmt('Unable to convert sprite %s due to colorkey failure!', [name]);
      blitSurface.ColorKey := image.ColorKey;

      if id = -1 then
         startingPoint := point(0, 0)
      else
         startingPoint := point((id mod sheet.X) * frame.X * sprite.X, (id div sheet.X) * frame.Y * sprite.Y);
      blitSurface.Fill(nil, blitSurface.ColorKey);
      for i := 0 to framesPerSprite - 1 do
      begin
         srcrect := rect(point(frame.X * (i mod sprite.X), frame.Y * (i div sprite.X)), frame);
         inc(srcrect.left, startingPoint.X);
         inc(srcrect.top, startingPoint.Y);
         dstrect := rect(point(0, i * frame.Y), frame);
         SDL_BlitSurface(image, @srcrect, blitSurface, @dstrect);
      end;
      convertedImage := saveToTBI(blitSurface);
      convertedImage.Seek(0, soFromBeginning);
      try
         writename := style + '\' + name;
         if id <> -1 then
            writename := writename + ' ' + intToStr(id);
         writename := writename + '.tbi';
         archiveInterface.GArchives[IMAGE_ARCHIVE].writeFile(writename, convertedImage);
      finally
         convertedImage.free;
      end;
   finally
      blitSurface.free;
   end;
   result := true;
end;

function convertSprite(name: string; id: integer): boolean;
const
   FRAME: TPoint = (X: 24; Y: 32);
   SPRITE: TPoint = (X: 3; Y: 4);
   SHEET: TPoint = (X: 4; Y: 2);
var
   oname: string;
   image: PSdlSurface;
begin
   result := false;
   oname := name;
   findGraphic(name, 'charset');
   if name = '' then
   begin
      logs.logText(format('Unable to locate charset image %s for conversion', [oname]));
      Exit;
   end;
   image := PSdlSurface(IMG_Load(PAnsiChar(Utf8String(name))));
   try
      result := convertImage(image, id, FRAME, SPRITE, SHEET, oname, 'mapsprite');
   finally
      image.free;
   end;
end;

function convertPortrait(name: string): boolean;
const
   FRAME: TPoint = (X: 48; Y: 48);
   SPRITE: TPoint = (X: 4; Y: 4);
   SHEET: TPoint = (X: 1; Y: 1);
var
   oname: string;
   image: PSdlSurface;
begin
   result := false;
   oname := name;
   findGraphic(name, 'faceset');
   if name = '' then
   begin
      logs.logText(format('Unable to locate portrait image %s for conversion', [oname]));
      Exit;
   end;
   image := PSdlSurface(IMG_Load(PAnsiChar(Utf8String(name))));
   try
      result := convertImage(image, -1, FRAME, SPRITE, SHEET, oname, 'portrait');
   finally
      image.free;
   end;
end;

function convertAnim(name: string): boolean;
const
   FRAME: TPoint = (X: 96; Y: 96);
   SHEET: TPoint = (X: 1; Y: 1);
var
   oname: string;
   image: PSdlSurface;
   sprite: TPoint;

begin
   result := false;
   oname := name;
   findGraphic(name, 'battle');
   if name = '' then
   begin
      logs.logText(format('Unable to locate battle anim image %s for conversion', [oname]));
      Exit;
   end;
   image := PSdlSurface(IMG_Load(PAnsiChar(Utf8String(name))));
   sprite := point(5, image.width div 96);
   try
      result := convertImage(image, -1, FRAME, sprite, SHEET, oname, 'animation');
   finally
      image.free;
   end;
end;

initialization
begin
   nameTable := nil;
end;

finalization
begin
   nameTable.free;
end;

end.