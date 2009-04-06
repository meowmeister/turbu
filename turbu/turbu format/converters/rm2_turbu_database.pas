unit rm2_turbu_database;
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
   LDB, turbu_database, turbu_unit_dictionary;

type
   TCallback = procedure of object;

   T2k2Database = class helper for TRpgDatabase
   private
      procedure buildNameLists(base: TLcfDatabase; onProgress: TCallback);
   public
      constructor convert(base: TLcfDatabase; dic: TUnitDictionary; onProgress: TCallback);
      function lookupMapSprite(name: ansiString; index: byte): integer;
      function lookupPortrait(name: ansiString; index: byte): integer;
   end;

var
   GLcfDatabase: TLcfDataBase;

implementation
uses
   sysUtils, classes, zlib,
   turbu_characters, turbu_items, turbu_skills, turbu_animations, conversion_table,
   turbu_resists,
   rm2_turbu_items, rm2_turbu_characters, rm2_turbu_skills, rm2_turbu_animations,
   rm2_turbu_resists, turbu_sprites, turbu_versioning, turbu_plugin_interface,
   turbu_constants, turbu_sdl_image, turbu_tbi_lib,
   hero_data, locate_files,
   archiveInterface, formats, commons, turbu_battle_engine, turbu_engines, logs,
   SDL_ImageManager, SDL, SDL_13;

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

procedure T2k2Database.buildNameLists(base: TLcfDatabase; onProgress: TCallback);
var
   counter: integer;
   dummy: string;
   i, j: integer;
//   converter: TMemoryStream;

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
         result.Add(dummy);
         inc(i);
         inc(j);
      end;
   end;

begin
   { Set up name table }
   nameTable.newDivision;
   counter := 0;
   for I := 1 to base.heroes do
      if not scanMapSprite(base.hero[i].sprite, base.hero[i].spriteIndex) then
         base.hero[i].sprite := '';
   onProgress;

   nameTable.newDivision;
   counter := 0;
   for I := 1 to base.heroes do
      if not scanPortrait(base.hero[i].portrait) then
         base.hero[i].portrait := '';
   onProgress;

   nameTable.newDivision;
   counter := 0;
   for I := 1 to base.anims do
      if not scanAnim(base.anim[i].filename) then
         base.anim[i].filename := '';
   onProgress;

   {worry about battlesprites later}

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

constructor T2k2Database.convert(base: TLcfDatabase; dic: TUnitDictionary; onProgress: TCallback);
var
   i, j: integer;
   counter, classes: integer;
   classTable, heroClassTable: TConversionTable;
   converter: TMemoryStream;
   reader: TStream;
   battleEngine: TBattleEngineData;
   engineArray: ^TBattleEngineArray;
   defMoveMatrix: TMoveMatrix;
   moveArray: ^TMoveMatrixArray;
begin
   //setup
   self.Create;
   GDatabase := self;
   GLcfDatabase := base;
   reader := GArchives[0].getFile('metadata');
   try
      self.algorithmLookup.loadFromStream(reader);
   finally
      reader.free;
   end;
   self.units.Free;
   self.units := dic;
   self.scriptBuild;
   self.parseMeta;
//   self.algorithms := algorithms;
   assert(SDL_WasInit(SDL_INIT_VIDEO) = SDL_INIT_VIDEO);

   //make sure required battle engine is available from plugins
   case GProjectFormat of
      pf_2k: battleEngine := TBattleEngineData(requireEngine(et_battle, 'First-person battle engine', TVersion.Create(0, 1, 1)));
      pf_2k3: battleEngine := TBattleEngineData(requireEngine(et_battle, 'Active-time battle engine', TVersion.Create(0, 1, 1)));
      else
      begin
         battleEngine := nil;
         assert(false);
      end;
   end;
   engineArray := @self.battleStyle;
   setLength(engineArray^, length(engineArray^) + 1);
   self.battleStyle[high(self.battleStyle)] := battleEngine;

   //define default move matrix
   setLength(defMoveMatrix, length(MOVE_MATRIX));
   for I := 0 to high(MOVE_MATRIX) do
   begin
      setLength(defMoveMatrix[i], length(MOVE_MATRIX[i]));
      for j := 0 to high(MOVE_MATRIX[i]) do
         defMoveMatrix[i, j] := MOVE_MATRIX[i, j];
   end;
   moveArray := @self.moveMatrix;
   setLength(moveArray^, length(moveArray^) + 1);
   self.moveMatrix[high(self.moveMatrix)] := defMoveMatrix;

   //create conversion tables
   classTable := TConversionTable.Create;
   heroClassTable := TConversionTable.Create;
   nametable.free;
   nametable := TNameTable.Create;
   converter := nil;

   try
      buildNameLists(base, onProgress);
      onProgress;

      //COMMANDS
      self.command.Add(TBattleCommand.Create);
      if GProjectFormat = pf_2k3 then
      begin
         for I := 1 to base.commands do
            command.add(TBattleCommand.convert(base.command[i], i));
      end else
      begin
         for I := 1 to 4 do
            command.Add(setup2kCommand(i));
         for I := 1 to base.heroes do
            if base.hero[i].hasSkillName then
            begin
               command.Add(TBattleCommand.Create);
               command.Last.id := command.High;
               command.Last.name := (string(base.hero[i].skillName));
               command.Last.style := cs_skillgroup;
               command.Last.value := command.High;
            end;
         //end FOR
      end;

      //CLASS RECORDS
      if GProjectFormat = pf_2k3 then
      begin
         self.charClasses := base.charClasses;
         counter := 0;
         for i := 1 to base.charClasses do
         begin
            if not isEmpty(base.charClass[i]) then
            begin
               charClass[i].Free;
               charClass[i] := TClassTemplate.convert(base.charClass[i]);
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
      for I := 1 to base.heroes do
      begin
         if (not isEmpty(base.hero[i])) and (base.hero[i].classNum = 0) then
         begin
            GDatabase.addClass(TClassTemplate.convert(base.hero[i], base));
            inc(counter);
            heroClassTable.add(i, counter);
         end;
      end;

      //HERO RECORDS
      for I := 1 to base.heroes do
         if base.hero[i].classNum <> 0 then
            GDatabase.addHero(THeroTemplate.convert(base.hero[i], classTable, base))
         else GDatabase.addHero(THeroTemplate.convert(base.hero[i], heroClassTable, base));
      onProgress;

      //ITEMS
      for I := 1 to base.items do
         TItemTemplate.addNewItem(base.item[i]);

      //LOAD ITEMS INTO CLASS/HERO RECORDS
      j := classes;
      for i := j + 1 to self.charClasses do
         if (not isEmpty(base.hero[i - classes])) and (base.hero[i - classes].classNum = 0) then
         begin
            inc(j);
            self.charClass[j].loadEq(base.hero[i - classes]);
         end;

      for I := 1 to base.heroes do
         self.hero[i].loadEq(base.hero[i]);
      onProgress;

      //SKILLS
      for I := 1 to base.skills do
         TSkillTemplate.addNewSkill(base.skill[i]);
      onProgress;

      //ANIMATIONS
      for i := 1 to base.anims do
         self.addAnim(TAnimTemplate.convert(base.anim[i], i));
      onProgress;

      //ATTRIBUTES
      for I := 1 to base.attributes do
         self.attributes.add(TAttributeTemplate.Convert(base.attribute[i], i));

      //CONDITIONS
      for I := 1 to base.conditions do
         self.conditions.Add(TConditionTemplate.Convert(base.condition[i], i));
      onProgress;

   finally
      classTable.Free;
      heroClassTable.Free;
      assert(converter = nil);
   end;
end;

function T2k2Database.lookupMapSprite(name: ansiString; index: byte): integer;
begin
   assert(assigned(nameTable));
   assert(index in [0..7]);
   result := nameTable.indexOf('mapsprite\' + unicodeString(name) + ' ' + intToStr(index), INDEX_SPRITE);
   if result < 0 then
      raise EConversionTableError.Create('Sprite name "' + unicodeString(name) + '" not found in conversion table!');
end;

function T2k2Database.lookupPortrait(name: ansiString; index: byte): integer;
begin
   assert(assigned(nameTable));
   assert(index in [0..15]);
   result := (nameTable.indexOf('portrait\' + unicodeString(name), INDEX_PORTRAIT) * 16) + index;
   if result < 0 then
      raise EConversionTableError.Create('Portrait name "' + unicodeString(name) + '" not found in conversion table!');
end;

{ Classless }

function convertImage(image: TRpgSdlImage; id: integer; frame, sprite, sheet: TPoint; style: string): boolean;
var
   blitSurface: PSdlSurface;
   framesPerSprite: integer;
   startingPoint: TPoint;
   i: integer;
   srcrect, dstrect: TSDL_Rect;
   convertedImage: TStream;
   writename: string;
begin
   framesPerSprite := SPRITE.X * SPRITE.Y;
   blitSurface := TSdlSurface.Create(FRAME.X, FRAME.Y * framesPerSprite, 8, 0, 0, 0, 0);
   try
      if not blitSurface.SetPalette(image.surface.format.palette.colors, 0, image.surface.format.palette.count) then
         raise ESdlImageException.Create('Unable to convert sprite ' + image.name + ' due to colorkey failure!');
      blitSurface.ColorKey := image.surface.colorkey;

      if id = -1 then
         startingPoint := point(0, 0)
      else startingPoint := point((id mod SHEET.X) * FRAME.X * SPRITE.X, (id div SHEET.X) * FRAME.Y * SPRITE.Y);
      blitSurface.Fill(nil, blitSurface.colorkey);
      for i := 0 to framesPerSprite - 1 do
      begin
         srcrect := rect(point(FRAME.X * (i mod SPRITE.X), FRAME.Y * (i div SPRITE.X)), FRAME);
         inc(srcrect.x, startingPoint.x);
         inc(srcrect.y, startingPoint.y);
         dstrect := rect(point(0, i * FRAME.Y), FRAME);
         SDL_BlitSurface(image.surface, @srcRect, blitSurface, @dstRect);
      end;
      convertedImage := saveToTBI(blitSurface);
      convertedImage.Seek(0, soFromBeginning);
      try
         writename := style + '\' + image.name;
         if id <> -1 then
            writename := writename + ' ' + intToStr(id);
         writename := writename + '.tbi';
         archiveInterface.GArchives[IMAGE_ARCHIVE].writeFile(writename, convertedImage);
      finally
         convertedImage.Free;
      end;
   finally
      blitSurface.Free;
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
   image: TRpgSdlImage;
begin
   result := false;
   oname := name;
   findGraphic(name, 'charset');
   if name = '' then
   begin
      logs.logText('Unable to locate charset image ' + oname + 'for conversion');
      Exit;
   end;
   image := TRpgSdlImage.Create(name, oname, nil);
   try
      result := convertImage(image, id, FRAME, SPRITE, SHEET, 'mapsprite');
   finally
      image.Free;
   end;
end;

function convertPortrait(name: string): boolean;
const
   FRAME: TPoint = (X: 48; Y: 48);
   SPRITE: TPoint = (X: 4; Y: 4);
   SHEET: TPoint = (X: 1; Y: 1);
var
   oname: string;
   image: TRpgSdlImage;
begin
   result := false;
   oname := name;
   findGraphic(name, 'faceset');
   if name = '' then
   begin
      logs.logText('Unable to locate portrait image ' + oname + 'for conversion');
      Exit;
   end;
   image := TRpgSdlImage.Create(name, oname, nil);
   try
      result := convertImage(image, -1, FRAME, SPRITE, SHEET, 'portrait');
   finally
      image.Free;
   end;
end;

function convertAnim(name: string): boolean;
const
   FRAME: TPoint = (X: 96; Y: 96);
   SHEET: TPoint = (X: 1; Y: 1);
var
   oname: string;
   image: TRpgSdlImage;
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
   image := TRpgSdlImage.Create(name, oname, nil);
   sprite := point(5, image.surface.width div 96);
   try
      result := convertImage(image, -1, FRAME, sprite, SHEET, 'animation');
   finally
      image.Free;
   end;
end;

initialization
begin
   nametable := nil;
end;

finalization
begin
   nameTable.free;
end;

end.
