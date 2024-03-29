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
   LDB, LMT, battle_anims, turbu_database, events, conversion_report,
   rm2_turbu_monsters, turbu_game_data,
   formats;

type

   T2k2Database = class helper for TRpgDatabase
   private
      procedure buildNameLists(base: TLcfDatabase;
         spriteList, portList, animList, animList2, battleSpriteList, weaponList: TStringList);
      procedure convertEvents(block: TEventBlock);
      procedure SaveScript(const script: utf8String);
      procedure ConvertVocab(base: TLcfDatabase);
      procedure AddVocab(const left: string; const right: AnsiString);
      procedure AddVocabCond(const left, formatString: string; base: TLcfDatabase;
        index: integer; cond: TProjectFormat);
      procedure ConvertVehicles(base: TSystemRecord);
      procedure ScanAnimSounds(base: TBattleAnim; list: TStringList);
      procedure ConvertSysSounds(base: TSystemRecord);
   public
      constructor convert(base: TLcfDatabase; tree: TFullTree;
                          ConversionReport: IConversionReport; scanner: TEventScanner;
                          spriteList, portList, animList, animList2, battleSpriteList,
                          weaponList, soundList: TStringList);
   end;

   T2k2Layout = class helper for TGameLayout
   public
      constructor Convert(base: TSystemRecord);
   end;

var
   GLcfDatabase: TLcfDatabase;

implementation
uses
   sysUtils, StrUtils, Generics.Defaults, Generics.Collections, Math,
   charset_data,
   turbu_characters, turbu_items, turbu_skills, turbu_animations, conversion_table,
   turbu_resists, turbu_map_metadata, turbu_sounds, rm2_turbu_sounds,
   rm2_turbu_items, rm2_turbu_characters, rm2_turbu_skills, rm2_turbu_animations,
   rm2_turbu_resists, rm2_turbu_map_metadata, rm2_turbu_tilesets, turbu_sprites,
   turbu_versioning, turbu_plugin_interface, turbu_constants, turbu_sdl_image,
   turbu_tbi_lib, turbu_tilesets, rm2_turbu_map_objects, turbu_map_objects,
   locate_files, turbu_maps, turbu_monsters, turbu_terrain, rm2_turbu_terrain,
   archiveInterface, commons, turbu_battle_engine, turbu_engines, logs,
   turbu_map_engine, EB_RpgScript, turbu_defs,
   SDL, SDL_13, sdl_image, sg_defs;

const
   MOVE_MATRIX: array[0..3, 0..3] of byte = ((1, 0, 1, 2), (4, 3, 4, 5), (7, 6, 7, 8), (10, 9, 10, 11));

{ T2k2Database }

procedure T2k2Database.buildNameLists(base: TLcfDatabase;
   spriteList, portList, animList, animList2, battleSpriteList, weaponList: TStringList);
var
   i, j: integer;
   v: TVehicleSet;
begin
   for i := 1 to base.heroes do
      spriteList.add(string(base.hero[i].sprite));

   for i := 1 to base.heroes do
      portList.Add(string(base.hero[i].portrait));

   for i := 1 to base.anims do
   begin
      if base.anim[i].largeAnim then
         animList2.Add(string(base.anim[i].filename))
      else animList.Add(string(base.anim[i].filename));
   end;

   for v := low(TVehicleSet) to high(TVehicleSet) do
      spriteList.Add(string(base.SystemData.vehicleGraphic[v]));

   for i := 1 to high(base.anim2) do
   begin
      for j := 1 to High(base.anim2[i].poses) do
         battleSpriteList.Add(string(base.anim2[i].poses[j].filename));
      for j := 1 to High(base.anim2[i].weapons) do
         weaponList.Add(string(base.anim2[i].weapons[j].filename));
   end;
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
   ConversionReport: IConversionReport; scanner: TEventScanner;
   spriteList, portList, animList, animList2, battleSpriteList, weaponList, soundList: TStringList);
var
   i, j: integer;
   counter, classes: integer;
   classTable, heroClassTable: TConversionTable;
   battleEngine: TBattleEngineData;
   defMoveMatrix: TMoveMatrix;
   moveArray: ^TMoveMatrixArray;
   newcmd: TBattleCommand;
   legacyPair: TPair<integer, AnsiString>;
begin
   // setup
   self.Create;
   GDatabase := self;
   GLcfDatabase := base;
   self.ConvertEvents(GlobalEventBlock as TEventBlock);
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
      buildNameLists(base, spriteList, portList, animList, animList2, battleSpriteList, weaponList);

      ConversionReport.newStep('Converting heroes');
      // COMMANDS
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
               newcmd := TBattleCommand.Create;
               newcmd.id := command.Count;
               newcmd.name := (string(base.hero[i].skillName));
               newcmd.style := cs_skillgroup;
               newcmd.value := command.Count;
               command.add(newcmd);
            end;
      end;

      // CLASS RECORDS
      if GProjectFormat = pf_2k3 then
      begin
         counter := 0;
         for i := 1 to base.charClasses do
         begin
            if not isEmpty(base.charClass[i]) then
            begin
               charClass.Add(TClassTemplate.convert(base.charClass[i], self.statSet));
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
            inc(counter);
            GDatabase.charClass.Add(TClassTemplate.convert(base.hero[i], base, self.statSet, counter));
            heroClassTable.add(i, counter);
         end;
      end;

      // HERO RECORDS
      for i := 1 to base.heroes do
         if base.hero[i].classNum <> 0 then
            GDatabase.Hero.Add(THeroTemplate.convert(base.hero[i], classTable, base, self.statSet))
         else GDatabase.Hero.Add(THeroTemplate.convert(base.hero[i], heroClassTable, base, self.statSet));

      ConversionReport.newStep('Converting Items');
      // ITEMS
      for i := 1 to base.items do
         TItemTemplate.addNewItem(base.item[i]);

      // LOAD ITEMS INTO CLASS/HERO RECORDS
      j := classes;
      for i := j + 1 to min(self.charClasses, counter - 1) do
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
      begin
         self.anim.Add(TAnimTemplate.convert(base.anim[i], i));
         ScanAnimSounds(base.anim[i], soundList);
      end;

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

      // MONSTERS
      ConversionReport.newStep('Converting Monsters');
      for i := 1 to base.monsters do
         self.monsters.add(TRpgMonster.convert(base.monster[i], i));

      // MONSTER PARTIES
      ConversionReport.newStep('Converting Monster Parties');
      ScanMPartiesForDuplicates(base.mparty, scanner);
      for i := 1 to base.mparties do
         self.monsterParties.add(TRpgMonsterParty.convert(base.mparty[i], i, scanner));
      ReportMPartyDuplicates(ConversionReport);

      // ANIM SECTION 2
      ConversionReport.newStep('Converting Battle Char data');
      for i := 1 to high(base.anim2) do
         self.battleChars.Add(TBattleCharAnim.convert(base.anim2[i], i));

      ConversionReport.newStep('Preparing layout');
      self.layout.Convert(base.SystemData);
      if GProjectFormat = pf_2k3 then
         self.layout.translucentMessages := base.battleLayout.windowTrans;
      ConvertVehicles(base.SystemData);
      ConvertSysSounds(base.SystemData);
      for legacyPair in base.SystemData.legacy do
         self.AddLegacy('SystemData', 0, legacyPair.key, legacyPair.value);

      ConversionReport.NewStep('Converting terrain');
      for i := 1 to base.terrains do
         self.terrains.add(TRpgTerrain.Convert(base.terrain[i], i));

      ConversionReport.newStep('Converting map tree');
      self.mapTree := TMapTree.convert(tree, true);

      for I := 1 to base.variables.len do
         self.variable.Add(string(base.variables.name[i]));
      assert(self.variable.Count = base.variables.len);

      for I := 1 to base.switches.len do
         self.switch.Add(string(base.switches.name[i]));
      assert(self.switch.Count = base.switches.len);

      ConvertVocab(base);
   finally
      classTable.free;
      heroClassTable.free;
   end;
end;

function SanitizeScriptName(const name: string): string;
const
   NAMES: array[0..96] of string = (
      'addExp', 'AddItem', 'AddLevels', 'AddTeleport', 'battle', 'battleCount', 'battleEx',
      'battles', 'changeTileset', 'characters', 'clearPortrait', 'deathPossible',
      'decreaseWeather', 'DeleteTeleport', 'DisableMenu', 'EnableMenu', 'EnableSave', 'eraseScreen',
      'fadeOutMusic', 'flashEvent', 'flashScreen', 'flees', 'GameOver', 'getObjectID',
      'getTerrainID', 'HeldItems', 'HeroCount', 'Heroes', 'heroJoin', 'heroLeave',
      'Image', 'increaseWeather', 'inn', 'inputNumber', 'inputText', 'itemCount', 'keyScan',
      'levelGainNotify', 'lockScreen', 'losses', 'MapObject', 'maps', 'media',
      'memorizeBgm', 'memorizeLocation', 'menuEnabled', 'messageOptions', 'messages',
      'money', 'newImage', 'OpenMenu', 'panScreen', 'panScreenTo', 'Party',
      'partySize', 'playMemorizedBgm', 'playMovie', 'playMusic', 'playSound',
      'Random', 'RemoveExp', 'RemoveItem', 'RemoveLevels', 'returnScreen',
      'rideVehicle', 'saveCount', 'SaveMenu', 'setBGImage', 'SetEncounterRate',
      'setPortrait', 'SetSkin', 'SetSystemMusic', 'SetSystemSound', 'settings',
      'setTransition', 'setWeather', 'shakeScreen', 'Shop', 'showBattleAnim',
      'showChoice', 'showMessage', 'showScreen', 'stopMoveScripts', 'swapEvents',
      'Switch', 'Teleport', 'teleportMapObject', 'teleportVehicle', 'thisObject',
      'timer', 'timer2', 'tintScreen', 'unlockScreen', 'Vehicle', 'victories',
      'wait', 'waitUntilMoved');
var
   dummy: integer;
begin
   if TArray.BinarySearch<string>(NAMES, name, dummy,
     TComparer<string>.construct(
       function(const l, r: string): integer
       begin result := StrIComp(PChar(l), PChar(r)) end)) then
      result := 'g' + name
   else result := name;
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
               script.Name := SanitizeScriptName(copy(script.name, 1, length(script.name) - 6))
            end);
         scriptName := obj.pages[0].scriptName;
         if AnsiEndsText('_page1', scriptName) then
            obj.pages[0].scriptName := SanitizeScriptName(copy(scriptName, 1, length(scriptName) - 6));
         obj.name := obj.pages[0].scriptName;
         GlobalEvents.Add(obj);
      end;
      self.saveScript(utf8String(FGlobalScriptBlock.serialize));
   finally
      nameList.Free;
   end;
end;

procedure T2k2Database.AddVocab(const left: string; const right: AnsiString);
begin
   self.FSysVocab.Values[left] := string(right);
end;

procedure T2k2Database.AddVocabCond(const left, formatString: string; base: TLcfDatabase;
  index: integer; cond: TProjectFormat);
begin
   if GProjectFormat = cond then
      AddVocab(left, AnsiString(format(formatString, [base.vocabDict[index]])))
   else AddVocab(left, '');
end;

type
   TVocabPair = record
      id: integer;
      name: string;
   end;

const
  BASE_VOCAB: array[TVocabSet] of string =
  (V_ITEMS_OWNED, V_ITEMS_EQUIPPED, V_MONEY_NAME, V_NORMAL_STATUS, V_STAT_EXP,
   V_STAT_SHORT_LV, V_STAT_SHORT_HP, V_STAT_SHORT_MP, V_MP_COST, V_STAT_ATTACK,
   V_STAT_DEFENSE, V_STAT_MIND, V_STAT_SPEED, V_EQ_WEAPON, V_EQ_SHIELD, V_EQ_ARMOR,
   V_EQ_HELMET, V_EQ_ACCESSORY);

  INN_FMT = 'Inn%d-%s';
  SHOP_VOCAB: array[TShopVocabSet] of string =
  (
   'Shop%d-Greet', 'Shop%d-Continue', 'Shop%d-Buy', 'Shop%d-Sell', 'Shop%d-Leave',
   'Shop%d-Buy What', 'Shop%d-Buy Quantity', 'Shop%d-Bought', 'Shop%d-Sell What',
   'Shop%d-Sell Quantity', 'Shop%d-Sold'
  );
  BATTLE_VOCAB: array [TBattleVocabSet] of string =
  (
   V_BATTLE_FIGHT, V_BATTLE_AUTO, V_BATTLE_FLEE, V_BATTLE_ATTACK,
   V_BATTLE_DEFEND, V_BATTLE_ITEM, V_BATTLE_SKILL
  );
  VOCAB_LIST: array[0..17] of TVocabPair = (
   (id: 2; name: 'Battle-Surprise Attack'), (id: 4; name: 'Battle-Failed Escape'),
   (id: 5; name: 'Battle-Victory'), (id: 6; name: 'Battle-Defeat'),
   (id: $6C; name: V_MENU_EQUIP), (id: $6E; name: V_MENU_SAVE),
   (id: $72; name: V_MENU_NEW), (id: $73; name: V_MENU_LOAD),
   (id: $75; name: V_MENU_QUIT), (id: $7B; name: V_STAT_LV),
   (id: $7C; name: V_STAT_HP), (id: $7D; name: V_STAT_MP),
   (id: $92; name: V_SAVE_WHERE), (id: $93; name: V_LOAD_WHERE),
   (id: $94; name: 'Save-File'), (id: $97; name: 'Confirm-Quit'),
   (id: $98; name: 'Confirm-Yes'), (id: $99; name: 'Confirm-No')
  );
  VOCAB_LIST_P: array[0..3] of TVocabPair = (
   (id: 7; name: 'Battle-Exp Gained'), (id: $A; name: 'Battle-Found Item'),
   (id: $24; name: 'Character-Level Up'), (id: $25; name: 'Character-Learned Skill')
  );
  VOCAB_LIST_2K: array[0..2] of TVocabPair = (
   (id: 3; name: 'Battle-Fled'), (id: $C; name: 'Battle-Ally Crit'),
   (id: $D; name: 'Battle-Enemy Crit')
  );
  VOCAB_LIST_2K_P: array[0..15] of TVocabPair = (
   (id: 1; name: 'Battle-Enemy Appears'), (id: $B; name: 'Battle-Ally Attacks'),
   (id: $E; name: 'Battle-Ally Defends'), (id: $F; name: 'Battle-Enemy Defends'),
   (id: $10; name: 'Battle-Enemy Building Strength'), (id: $11; name: 'Battle-Explodes'),
   (id: $12; name: 'Battle-Enemy Flees'), (id: $13; name: 'Battle-Enemy Transforms'),
   (id: $14; name: 'Battle-Enemy Injured'), (id: $15; name: 'Battle-Enemy Missed'),
   (id: $16; name: 'Battle-Ally Injured'), (id: $17; name: 'Battle-Ally Missed'),
   (id: $18; name: 'Battle-Skill Failed 1'), (id: $19; name: 'Battle-Skill Failed 2'),
   (id: $1A; name: 'Battle-Skill Failed 3'), (id: $1B; name: 'Battle-Dodge')
  );
  VOCAB_LIST_2K_PS: array[0..7] of TVocabPair = (
   (id: $1C; name: 'Battle-Uses Item'), (id: $1D; name: 'Battle-Recovery'),
   (id: $1E; name: 'Battle-Ability Up'), (id: $1F; name: 'Battle-Ability Down'),
   (id: $20; name: 'Battle-Ally Absorb'), (id: $21; name: 'Battle-Enemy Absorb'),
   (id: $22; name: 'Battle-Defense Up'), (id: $23; name: 'Battle-Defense Down')
  );
  VOCAB_LIST_2K3: array[0..7] of TVocabPair = (
   (id: $26; name: 'Battle-Begin'), (id: $27; name: 'Battle-Miss'),
   (id: $70; name: 'Menu-Quit 2k3'),
   (id: $76; name: 'Menu-Status'), (id: $77; name: 'Menu-Row'),
   (id: $78; name: 'Menu-Order'),
   (id: $79; name: 'ATB-Wait'), (id: $7A; name: 'ATB-Active')
  );

procedure T2k2Database.ConvertVocab(base: TLcfDatabase);
var
   vocab: TVocabSet;
   shops: TShopVocabSet;
   battle: TBattleVocabSet;
   i: integer;
   pair: TVocabPair;
begin
   for vocab := low(TVocabSet) to High(TVocabSet) do
      AddVocab(BASE_VOCAB[vocab], base.vocabulary[vocab]);
   for I := 1 to INN_STYLES do
   begin
      AddVocab(format(INN_FMT, [i, 'Greet']),
               AnsiString(format('%s\i\$%s'#13#10'%s', [base.innVocab[i, inn_greet1], base.innVocab[i, inn_greet2], base.innVocab[i, inn_greet3]])));
      AddVocab(format(INN_FMT, [i, 'Stay']), base.innVocab[i, inn_stay]);
      AddVocab(format(INN_FMT, [i, 'Cancel']), base.innVocab[i, inn_cancel]);
   end;
   for i := 1 to SHOP_STYLES do
      for shops := low(TShopVocabSet) to high(TShopVocabSet) do
         AddVocab(format(SHOP_VOCAB[shops], [i]), base.shopVocab[i, shops]);
   for battle := Low(TBattleVocabSet) to High(TBattleVocabSet) do
      AddVocab(BATTLE_VOCAB[battle], base.battleVocab[battle]);
   AddVocab('Battle-Found Gold', AnsiString(format('%s\i%s', [base.vocabDict[8], base.vocabDict[9]])));
   for pair in VOCAB_LIST do
      AddVocab(pair.name, base.vocabDict[pair.id]);
   for pair in VOCAB_LIST_P do
      AddVocab(pair.name, AnsiString(format('\i%s', [base.vocabDict[pair.id]])));
   for pair in VOCAB_LIST_2K do
      AddVocabCond(pair.name, '%s', base, pair.id, pf_2k);
   for pair in VOCAB_LIST_2K_P do
      AddVocabCond(pair.name, '\i%s', base, pair.id, pf_2k);
   for pair in VOCAB_LIST_2K_PS do
      AddVocabCond(pair.name, '\i1%s\i2', base, pair.id, pf_2k);
   for pair in VOCAB_LIST_2K3 do
      AddVocabCond(pair.name, '%s', base, pair.id,  pf_2k3);
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
      filename := GArchives[SCRIPT_ARCHIVE].MakeValidFilename('', 'globalevents.trs');
      GArchives[SCRIPT_ARCHIVE].writeFile(filename.name, stream);
      self.scriptFile := filename.name;
   finally
      stream.Free;
   end;
end;

procedure T2k2Database.ScanAnimSounds(base: TBattleAnim; list: TStringList);
var
   i: integer;
begin
   for i := 1 to base.effects do
     list.Add(ExtractFileName(ChangeFileExt(string(base.effect[i].sound.filename), '')));
end;

function ConvertVehicle(base: TSystemRecord; vehicle: TVehicleSet): TVehicleTemplate;
begin
   result := TVehicleTemplate.Create;
   result.id := ord(vehicle) + 1;
   result.mapSprite := format('%s %d', [string(base.vehicleGraphic[vehicle]), base.vehicleIndex[vehicle]]);
   result.translucent := false;
   case vehicle of
      vh_boat:
      begin
         result.name := 'Boat';
         result.shallowWater := true;
         result.movementStyle := msSurface;
         result.music.Free;
         result.music := TRpgMusic.Convert(base.bgm[bgmBoat]);
      end;
      vh_ship:
      begin
         result.name := 'Ship';
         result.shallowWater := true;
         result.deepWater := true;
         result.movementStyle := msSurface;
         result.music.Free;
         result.music := TRpgMusic.Convert(base.bgm[bgmShip]);
      end;
      vh_airship:
      begin
         result.name := 'Airship';
         result.movementStyle := msFly;
         result.altitude := 16;
         result.music.Free;
         result.music := TRpgMusic.Convert(base.bgm[bgmAirship]);
      end;
   end;
end;

procedure T2k2Database.ConvertVehicles(base: TSystemRecord);
var
   vehicle: TVehicleSet;
begin
   for vehicle := Low(TVehicleSet) to High(TVehicleSet) do
      self.vehicles.add(ConvertVehicle(base, vehicle));
end;

procedure T2k2Database.ConvertSysSounds(base: TSystemRecord);
const TABLE: array[TBgmTypes] of LDB.TBgmTypes =
  (LDB.bgmBattle, LDB.bgmVictory, LDB.bgmInn, LDB.bgmGameOver, LDB.bgmTitle, LDB.bgmBossBattle);
var
   bgm: TBgmTypes;
   sfx: TSfxTypes;
begin
  for bgm := Low(TBgmTypes) to High(TBgmTypes) do
  begin
    FBgm[bgm] := (TRpgMusic.Convert(base.bgm[TABLE[bgm]]));
    FBgm[bgm].id := ord(bgm);
  end;
  for sfx := Low(TSfxTypes) to High(TSfxTypes) do
  begin
    FSfx[sfx] := (TRpgSound.Convert(base.sfx[LDB.TSfxTypes(sfx)]));
    FSfx[sfx].id := ord(sfx);
  end;
end;

{ T2k2Layout }

constructor T2k2Layout.Convert(base: TSystemRecord);
var
   i: integer;
begin
   FWidth := LOGICAL_SIZE.X;
   FHeight := LOGICAL_SIZE.Y;
   FPWidth := PHYSICAL_SIZE.X;
   FPHeight := PHYSICAL_SIZE.Y;

   FTitleScreen := string(base.titleScreen);
   FGameOverScreen := string(base.gameOverScreen);
   FSysGraphic := string(base.systemGraphic);
   FBattleSysGraphic := string(base.battleSysGraphic);
   FEditorBattleBG := string(base.editorBattleBG);
   FWallpaperStretch := not base.wallpaperStretch; //original was false = stretch, true = tiled
   FWhichFont := base.font;
   FStartingHeroes := base.startingHeroes;
   for i := 1 to 4 do
      FStartingHero[i] := base.startingHero[i];
   for i := ord(Low(TTransitionTypes)) to ord(High(TTransitionTypes)) do
      FTransition[TTransitionTypes(i)] := base.transition[TTransitionTypes(i)];
   for i := 0 to High(base.defaultCommands) do
      FCommands[i + 1] := base.defaultCommands[i];
   FUsesFrame := base.usesFrame;
   FFrame := string(base.frame);
   FReverseGraphics := base.reverseGraphics;
end;

end.
