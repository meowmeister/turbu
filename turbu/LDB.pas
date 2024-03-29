unit LDB;
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
   types, classes, Generics.Collections, //system libraries
   chipset, hero_data, item_data, skill_data, rm_sound, condition_data,
   events, charset_data, monster, battle_anims, turbu_defs, LMT; //modules

const
   SHOP_STYLES = 3;
   INN_STYLES = 2;

type
   TBgmTypes = (bgmTitle, bgmBattle, bgmBossBattle, bgmVictory, bgmInn,
                bgmBoat, bgmShip, bgmAirship, bgmGameOver);
   TSfxTypes = (sfxCursor, sfxAccept, sfxCancel, sfxBuzzer, sfxBattleStart,
                sfxEscape, sfxEnemyAttack, sfxEnemyDamage, sfxAllyDamage,
                sfxEvade, sfxEnemyDies, sfxItemUsed);
   TVocabSet = (ownedItems, equippedItems, moneyUnit, normalStatus, expShort,
                lvShort, hpShort, mpShort, mpCost, attack, defense, mind, speed,
                weapon, shield, armor, helmet, relic);
   TShopVocabSet = (shp_greet, shp_regreet, shp_buy, shp_sell, shp_leave,
                shp_buyWhat, shp_buyQty, shp_bought, shp_sellWhat, shp_sellQty,
                shp_sold);
   TInnVocabSet = (inn_greet1, inn_greet2, inn_greet3, inn_stay, inn_cancel);
   TBattleVocabSet = (bv_fight, bv_autobattle, bv_flee, bv_attack, bv_defend, bv_item, bv_skill);

   TVocabDict = TDictionary<integer, AnsiString>;
   TLegacyDict = TVocabDict;
   TWordArray = array of word;

   TAttribute = class(TObject)
   private
      FName: ansiString;
      FWeaponRestrict: boolean;
      FRateA: smallint;
      FRateB: smallint;
      FRateC: smallint;
      FRateD: smallint;
      FRateE: smallint;
      function getRate(value: byte): smallint;
   public
      constructor Create(theLDB: TStream; const id: word);

      property name: ansiString read FName;
      property weaponRestrict: boolean read FWeaponRestrict;
      property rate[value: byte]: smallint read getRate;
   end;

   TVarSection = class(TObject)
   private
      FLength: word;
      FVarNames: array of ansiString;

      function getName(x: word): ansiString;
      procedure setName(x: word; data: ansiString);
   public
      constructor create(input: TStream; size: word);
      property len: word read FLength;
      property name[x: word]: ansiString read getName write setname;
   end;

   TSwitchSection = class(TObject)
   private
      FLength: word;
      FSwitchNames: array of ansiString;

      function getName(x: word): ansiString;
      procedure setName(x: word; data: ansiString);
   public
      constructor create(input: TStream; size: word);
      property len: word read FLength;
      property name[x: word]: ansiString read getName write setname;
   end;

   TSystemRecord = class(TObject)
   private
      FVehicleGraphic: array[TVehicleSet] of ansiString;
      FVehicleIndex: array[TVehicleSet] of byte;
      FTitleScreen: ansiString;
      FGameOverScreen: ansiString;
      FSysGraphic: ansiString;
      FBattleSysGraphic: ansiString;
      FEditorBattleBG: ansiString;
      FWallpaperStretch: boolean;
      FWhichFont: byte;
      FBgm: array[TBgmTypes] of TRmMusic;
      FSfx: array[TSfxTypes] of TRmSound;
      FStartingHeroes: word;
      FStartingHero: array[1..4] of word;
      FTransition: array[TTransitionTypes] of byte;
      FCommands: TWordArray;
      FLegacy: TLegacyDict;
      FUsesFrame: boolean;
      FFrame: ansiString;
      FReverseGraphics: boolean;

      function getTransition(which: TTransitionTypes): byte;
      procedure setTransition(which: TTransitionTypes; const Value: byte);
      function getSfx(which: TSfxTypes): TRmSound;
      procedure setSfx(which: TSfxTypes; const Value: TRmSound);
      function getBgm(which: TBgmTypes): TRmMusic;
      procedure setBgm(which: TBgmTypes; const Value: TRmMusic);
      function getVehicleIndex(which: TVehicleSet): byte;
      procedure setVehicleIndex(which: TVehicleSet; const Value: byte);
      function getVehicleGraphic(which: TVehicleSet): ansiString;
      procedure setVehicleGraphic(which: TVehicleSet; const Value: ansiString);
      function getStartingHero(which: word): word;
   public
      constructor Create(input: TStream);
      destructor Destroy; override;
      property vehicleGraphic[which: TVehicleSet]: ansiString read getVehicleGraphic write setVehicleGraphic;
      property vehicleIndex[which: TVehicleSet]: byte read getVehicleIndex write setVehicleIndex;
      property systemGraphic: ansiString read FSysGraphic;
      property wallpaperStretch: boolean read FWallpaperStretch;
      property battleSysGraphic: ansiString read FBattleSysGraphic;
      property editorBattleBG: ansiString read FEditorBattleBG;
      property bgm[which: TBgmTypes]: TRmMusic read getBgm write setBgm;
      property sfx[which: TSfxTypes]: TRmSound read getSfx write setSfx;
      property transition[which: TTransitionTypes]: byte read getTransition write setTransition;
      property startingHeroes: word read FStartingHeroes;
      property startingHero[which: word]: word read getStartingHero;
      property titleScreen: ansiString read FTitleScreen;
      property gameOverScreen: ansiString read FGameOverScreen;
      property defaultCommands: TWordArray read FCommands;
      property usesFrame: boolean read FUsesFrame;
      property frame: ansiString read FFrame;
      property reverseGraphics: boolean read FReverseGraphics;
      property font: byte read FWhichFont;
      property legacy: TLegacyDict read FLegacy;
   end;

   TCommandStyle = (cs_weapon, cs_anyskill, cs_skillgroup, cs_defend, cs_item, cs_flee, cs_special);
   TVehiclePassSet = array[TVehicleSet] of boolean;

   TTerrainInfo = class(TObject)
   private
      FName: ansiString;
      FDamage: shortint;
      FEncounterMultiplier: word;
      FBattleBg: ansiString;
      FVehiclePass: TVehiclePassSet;
      FAirshipLanding: boolean;
      FConcealment: TConcealmentFactor;
      FSoundEffect: TRmSound;
      FFrame: ansiString;
      FLegacy: TLegacyDict;

      function vehicleCanPass(which: TVehicleSet): boolean; inline;
   public
      constructor Create(input: TStream; const id: word);
      destructor Destroy; override;

      property name: ansiString read FName;
      property damage: shortint read FDamage;
      property encounterMultiplier: word read FEncounterMultiplier;
      property battleBg: ansiString read FBattleBg;
      property vehiclePass[which: TVehicleSet]: boolean read vehicleCanPass;
      property airshipLanding: boolean read FAirshipLanding;
      property Concealment: TConcealmentFactor read FConcealment;
      property soundEffect: TRmSound read FSoundEffect;
      property frame: ansiString read FFrame;
      property legacy: TLegacyDict read FLegacy;
   end;

   TVocabulary = array [TVocabSet] of ansiString;
   TShopVocabulary = array[1..SHOP_STYLES, TShopVocabSet] of ansiString;
   TInnVocabulary = array[1..INN_STYLES, TInnVocabSet] of ansiString;
   TBattleVocabulary = array[TBattleVocabSet] of ansiString;

   TBattleCommand = class(TObject)
   private
      FName: ansiString;
      FStyle: TCommandStyle;
   public
      constructor Create(input: TStream; const id: word);

      property name: ansiString read FName;
      property style: TCommandStyle read FStyle;
   end;

   TBattleLayout = class(TObject)
   private
      FAutoLineup: boolean; //02
      FDeathEvent: boolean; //04
      FBattleStyle: byte; //07
      FCommands: array of TBattleCommand; //0A
      FUsesDeathEventHandler: boolean; //0F
      FDeathEventHandler: word; //10
      FWindowSize: byte; //14; 0: large; 1: small
      FWindowTrans: boolean; //18
      FTeleportOnDeath: boolean; //19
      FEscapeMap: word; //1A
      FEscapePoint: TPoint; //1B, 1C
      FUnk1D: integer;
   public
      constructor Create(input: TStream);
      destructor Destroy; override;

      property autoLineup: boolean read FAutoLineup;
      property hasDeathEvent: boolean read FDeathEvent;
      property deathEvent: word read FDeathEventHandler;
      property battleStyle: byte read FBattleStyle;
      property windowSize: byte read FWindowSize;
      property windowTrans: boolean read FWindowTrans;
      property teleportOnDeath: boolean read FTeleportOnDeath;
      property escapeMap: word read FEscapeMap;
      property escapePoint: TPoint read FEscapePoint;
      property unk1D: integer read FUnk1D;
   end;

   TLcfDataBase = class (TObject)
   private
      FHeroes: array of THeroRecord;
      FSkills: word;
      FSkill: array of TSkill;
      FItems: word;
      FItem: array of TItem;
      FAttributes: word;
      FAttribute: array of TAttribute;
      FConditions: word;
      FCondition: array of TCondition;
      FBattleAnims: word;
      FBattleAnim: array of TBattleAnim;
      FTerrains: word;
      FTerrain: array of TTerrainInfo;
      chipSets: word;
      chipSet: array of TChipSet;
      FVocabulary: TVocabulary;
      FShopVocabulary: TShopVocabulary;
      FInnVocabulary: TInnVocabulary;
      FBattleVocabulary: TBattleVocabulary;
      FVocabDict: TVocabDict;
      FSystemData: TSystemRecord;
      FSwitches: TSwitchSection;
      FVariables: TVarSection;
      FBattleLayout: TBattleLayout;
      FClasses: word;
      FClass: array of TRm2CharClass;
      FMonster: array of TMonster;
      FMParty: TArray<TMonsterParty>;
      FAnim2: TArray<TBattleAnim2>;

      function getTerrain(x: word): TTerrainInfo;
      function getAttribute(x: word): TAttribute;
      function getConditionCount: word;
      function getHero(x: word): THeroRecord;
      function heroCount: word;
      function getItem(x: word): TItem;
      function getSkill(x: word): TSkill;
      function getSkillCount: word;
      function getCondition(x: word): TCondition;
      function getAnim(x: word): TBattleAnim;
      function getClass(x: word): TRm2CharClass;
      function getClassCount: word;
      function getCommands: word;
      function getCommand(x: word): TBattleCommand;
      procedure AddVocabRange(theLDB: TStream; low, high: integer);
      procedure AddVocab(theLDB: TStream; index: integer);
      function getMonster(x: word): TMonster;
      function getMonsters: word;
      function getMonsterParties: word;
   public
      constructor Create(theLDB: TStream);
      destructor Destroy; override;
      function getMaxChipsets: word;
      function getChipset(id: word): TChipSet;
      function seekBlankChipset: word;

      property hero[x: word]: THeroRecord read getHero;
      property heroes: word read heroCount;
      property variables: TVarSection read FVariables write FVariables;
      property switches: TSwitchSection read FSwitches write FSwitches;
      property items: word read FItems;
      property item[x: word]: TItem read getItem;
      property skill[x: word]: TSkill read getSkill;
      property skills: word read getSkillCount;
      property condition[x: word]: TCondition read getCondition;
      property conditions: word read getConditionCount;
      property terrain[x: word]: TTerrainInfo read getTerrain;
      property terrains: word read FTerrains;
      property attribute[x: word]: TAttribute read getAttribute;
      property attributes: word read FAttributes;
      property anim[x: word]: TBattleAnim read getAnim;
      property anims: word read FBattleAnims;
      property SystemData: TSystemRecord read FSystemData;
      property vocabulary: TVocabulary read FVocabulary;
      property shopVocab: TShopVocabulary read FShopVocabulary;
      property innVocab: TInnVocabulary read FInnVocabulary;
      property battleVocab: TBattleVocabulary read FBattleVocabulary;
      property vocabDict: TVocabDict read FVocabDict;
      property charClasses: word read getClassCount;
      property charClass[x: word]: TRm2CharClass read getClass;
      property commands: word read getCommands;
      property command[x: word]: TBattleCommand read getCommand;
      property monsters: word read getMonsters;
      property monster[x: word]: TMonster read getMonster;
      property mparties: word read getMonsterParties;
      property mparty: TArray<TMonsterParty> read FMparty;
      property anim2: TArray<TBattleAnim2> read FAnim2;
      property battleLayout: TBattleLayout read FBattleLayout;
   end;

   function globalEventBlock: TObject;

implementation

uses
   sysUtils, windows, //system libs
   commons, fileIO, BER, formats; //turbu libs

var
   FGlobalEvents: TEventBlock;

procedure fillInSwitchStr(const expected: byte; out theResult: ansiString); forward;
procedure fillInAttribInt(const expected: byte; out theResult: integer); forward;
procedure fillInTerrainInt(const expected: byte; out theResult: integer); forward;
procedure fillInSysRecordInt(const expected: byte; out theResult: integer); forward;

procedure TLcfDataBase.AddVocab(theLDB: TStream; index: integer);
begin
   if NextSecIs(TheLDB, index) then
      FVocabDict.Add(index, getStrSec(index, theLDB, nil));
end;

procedure TLcfDataBase.AddVocabRange(theLDB: TStream; low, high: integer);
var
   i: integer;
begin
   for i := low to high do
      AddVocab(theLDB, i);
end;

constructor TLcfDataBase.Create(theLDB: TStream);
var
   converter: intX80;
   i, whichSet: word;
   savedPos: integer;
   whichShopVocab: TShopVocabSet;
   whichInnVocab: TInnVocabSet;
begin
   inherited Create;
try
   if not PeekAhead(theLDB, $0b) then //hero section
      raise EParseMessage.create('Hero section of RPG_RT.LDB not found!');
   converter := TBerConverter.Create(theLDB); // read the length statement
   converter.read(theLDB); // this one is the number of heroes
   setLength(FHeroes, converter.getData + 1);
   FHeroes[0] := nil;
   for i := 1 to length(FHeroes) - 1 do
      FHeroes[i] := THeroRecord.Create(theLDB, i, self);

   if GProjectFormat = pf_2k3 then  //must read classes before loading items
   begin
      savedPos := theLDB.Position;
      for i := $C to $1D do
         skipSec(i, theLDB);

      //class section
      if not PeekAhead(theLDB, $1E) then
         raise EParseMessage.create('Class section of RPG_RT.LDB not found!');
      converter.read(theLDB); // read the length statement
      converter.read(theLDB); // this one is the number of class records
      FClasses := converter.getData;
      //0-index array fix
      setLength(FClass, FClasses + 1);
      FClass[0] := nil;
      for i := 1 to FClasses do
         FClass[i] := TRm2CharClass.Create(theLDB, i, self);

      theLDB.Seek(savedPos, soFromBeginning);
   end;

   //skills section
   if not PeekAhead(theLDB, $0c) then
      raise EParseMessage.create('Skill section of RPG_RT.LDB not found!');
   converter.read(theLDB); // read the length statement
   converter.read(theLDB); // this one is the number of items
   FSkills := converter.getData;
   //0-index array fix
   setLength(FSkill, FSkills + 1);
   FSkill[0] := nil;
   for i := 1 to FSkills do
      FSkill[i] := TSkill.Create(theLDB, i, self);

   //items section
   if not PeekAhead(theLDB, $0d) then
      raise EParseMessage.create('Item section of RPG_RT.LDB not found!');
   converter.read(theLDB); // read the length statement
   converter.read(theLDB); // this one is the number of items
   FItems := converter.getData;
   //0-index array fix
   setLength(FItem, FItems + 1);
   FItem[0] := nil;
   for i := 1 to FItems do
      FItem[i] := TItem.Create(theLDB, i, self);

   //monsters section
   if not PeekAhead(theLDB, $0e) then
      raise EParseMessage.create('Monster section of RPG_RT.LDB not found!');
   converter.read(theLDB); // read the length statement
   converter.read(theLDB); // this one is the number of items
   //0-index array fix
   setLength(FMonster, converter.getData + 1);
   FMonster[0] := nil;
   for i := 1 to high(FMonster) do
      FMonster[i] := TMonster.Create(theLDB, i);

   //monster parties section
   if not PeekAhead(theLDB, $0f) then
      raise EParseMessage.create('Monster parties section of RPG_RT.LDB not found!');
   converter.read(theLDB); // read the length statement
   converter.read(theLDB); // this one is the number of items
   //0-index array fix
   setLength(FMparty, converter.getData + 1);
   FMParty[0] := nil;
   for i := 1 to high(FMParty) do
      FMParty[i] := TMonsterParty.Create(theLDB, i);

   if not peekAhead(theLDB, $10) then //terrain section
      raise EParseMessage.create('Terrain data section of RPG_RT.LDB not found!');
   converter.read(theLDB); //length
   converter.read(theLDB); //quantity
   FTerrains := converter.getData;
   setLength(FTerrain, FTerrains + 1);
   FTerrain[0] := nil;
   for I := 1 to FTerrains do
      FTerrain[i] := TTerrainInfo.Create(theLDB, i);

   if not peekAhead(theLDB, $11) then //Attributes section
      raise EParseMessage.create('Attribute section of RPG_RT.LDB not found!');
   converter.read(theLDB); //length statement
   converter.read(theLDB); //quantity
   FAttributes := converter.getData;
   setLength(FAttribute, FAttributes + 1);
   FAttribute[0] := nil;
   for I := 1 to FAttributes do
      FAttribute[i] := TAttribute.Create(theLDB, i);

   //Conditions section
   if not PeekAhead(theLDB, $12) then
      raise EParseMessage.create('Condition section of RPG_RT.LDB not found!');
   converter.read(theLDB); // read the length statement
   converter.read(theLDB); // this one is the number of items
   FConditions := converter.getData;
   //0-index array fix
   setLength(FCondition, FConditions + 1);
   FCondition[0] := nil;
   for i := 1 to FConditions do
      FCondition[i] := TCondition.Create(theLDB, i);

   //revise condition settings for heroes
   for i := 1 to high(FHeroes) do
      FHeroes[i].reviseConditions(converter.getData + 1);
   for i := 1 to high(FSkill) do
      FSkill[i].reviseConditions(converter.getData + 1);
   for i := 1 to high(FItem) do
      FItem[i].reviseConditions(converter.getData + 1);
   if GProjectFormat = pf_2k3 then
   begin
      for i := 1 to high(FClass) do
         FClass[i].reviseConditions(converter.getData + 1);
   end;

   //Animations section
   if not PeekAhead(theLDB, $13) then
      raise EParseMessage.create('Animation section of RPG_RT.LDB not found!');
   converter.read(theLDB); // read the length statement
   converter.read(theLDB); // this one is the number of items
   FBattleAnims := converter.getData;
   //0-index array fix
   setLength(FBattleAnim, FBattleAnims + 1);
   FBattleAnim[0] := nil;
   for i := 1 to FBattleAnims do
      FBattleAnim[i] := TBattleAnim.Create(theLDB, i);

   //chipset section
   if not PeekAhead(theLDB, $14) then
      raise EParseMessage.create('Chipset section of RPG_RT.LDB not found!');
   converter.read(theLDB); // read the length statement
   converter.read(theLDB); // this one is the number of chipsets
   chipSets := converter.getData;
   //0-index array fix
   setLength(chipSet, chipSets + 1);
   chipSet[0] := nil;
   for i := 1 to chipSets do
      chipSet[i] := TChipSet.Create(theLDB, i);

   //VOCAB SECTION
   if not peekAhead(theLDB, $15) then
      raise EParseMessage.create('System Vocabulary section of RPG_RT.LDB not found!');
   FVocabDict := TVocabDict.Create(converter.create(theLDB).getData * 2);
   AddVocabRange(theLDB, 1, $27);
   i := $29;
   for whichSet := 1 to SHOP_STYLES do
   begin
      for whichShopVocab := low(TShopVocabSet) to high(TShopVocabSet) do
      begin
         FShopVocabulary[whichSet, whichShopVocab] := getStrSec(i, theLDB, fillInBlankStr);
         inc(i);
      end;
      inc(i, 2);
   end;
   for whichSet := 1 to INN_STYLES do
   begin
      for whichInnVocab := low(TInnVocabSet) to high(TInnVocabSet) do
      begin
         FInnVocabulary[whichSet, whichInnVocab] := getStrSec(i, theLDB, fillInBlankStr);
         inc(i);
      end;
   end;
   FVocabulary[ownedItems] := getStrSec($5C, theLDB, fillInBlankStr);
   FVocabulary[equippedItems] := getStrSec($5D, theLDB, fillInBlankStr);
   FVocabulary[moneyUnit] := getStrSec($5F, theLDB, fillInBlankStr);
   FBattleVocabulary[bv_fight] := getStrSec($65, theLDB, fillInBlankStr);
   FBattleVocabulary[bv_autobattle] := getStrSec($66, theLDB, fillInBlankStr);
   FBattleVocabulary[bv_flee] := getStrSec($67, theLDB, fillInBlankStr);
   if GProjectFormat = pf_2k then
   begin
      FBattleVocabulary[bv_attack] := getStrSec($68, theLDB, fillInBlankStr);
      FBattleVocabulary[bv_defend] := getStrSec($69, theLDB, fillInBlankStr);
   end
   else begin
      skipSec($68, theLDB);
      skipSec($69, theLDB);
   end;
   FBattleVocabulary[bv_item] := getStrSec($6A, theLDB, fillInBlankStr);
   FBattleVocabulary[bv_skill] := getStrSec($6B, theLDB, fillInBlankStr);

   AddVocabRange(theLDB, $6C, $7D);

   FVocabulary[normalStatus] := getStrSec($7E, theLDB, fillInBlankStr);
   FVocabulary[expShort] := getStrSec($7F, theLDB, fillInBlankStr);
   FVocabulary[lvShort] := getStrSec($80, theLDB, fillInBlankStr);
   FVocabulary[hpShort] := getStrSec($81, theLDB, fillInBlankStr);
   FVocabulary[mpShort] := getStrSec($82, theLDB, fillInBlankStr);
   FVocabulary[mpCost] := getStrSec($83, theLDB, fillInBlankStr);
   FVocabulary[attack] := getStrSec($84, theLDB, fillInBlankStr);
   FVocabulary[defense] := getStrSec($85, theLDB, fillInBlankStr);
   FVocabulary[mind] := getStrSec($86, theLDB, fillInBlankStr);
   FVocabulary[speed] := getStrSec($87, theLDB, fillInBlankStr);
   FVocabulary[weapon] := getStrSec($88, theLDB, fillInBlankStr);
   FVocabulary[shield] := getStrSec($89, theLDB, fillInBlankStr);
   FVocabulary[armor] := getStrSec($8A, theLDB, fillInBlankStr);
   FVocabulary[helmet] := getStrSec($8B, theLDB, fillInBlankStr);
   FVocabulary[relic] := getStrSec($8C, theLDB, fillInBlankStr);

   AddVocabRange(theLDB, $92, $99);
   assert(peekAhead(theLdb, 0));

   //System Data
   if not PeekAhead(theLDB, $16) then
      raise EParseMessage.create('System Info section of RPG_RT.LDB not found!');
   converter.read(theLDB); // bypass the length statement
   FSystemData := TSystemRecord.create(theLDB);

   //switch section
   if not PeekAhead(theLDB, $17) then
      raise EParseMessage.create('Switch section of RPG_RT.LDB not found!');
   converter.read(theLDB); // bypass the length statement
   converter.read(theLDB); // this one is the number of switches
   FSwitches := TSwitchSection.create(theLDB, converter.getData);

//variable section
   if not PeekAhead(theLDB, $18) then
      raise EParseMessage.create('Variable section of RPG_RT.LDB not found!');
   converter.read(theLDB); // bypass the length statement
   converter.read(theLDB); // this one is the number of variables
   FVariables := TVarSection.create(theLDB, converter.getData);

//common event section
   if not PeekAhead(theLDB, $19) then
      raise EParseMessage.create('Variable section of RPG_RT.LDB not found!');
   converter.read(theLDB); // bypass the length statement
   FGlobalEvents := TEventBlock.create(theLDB);

   if GProjectFormat = pf_2k3 then
   begin
      //sections $1A-$1C (and 1F, below) are apparently always empty.
      //Why are they in there?
      assert(PeekAheadW(theLDB, $001A));
      assert(PeekAheadW(theLDB, $001B));
      assert(PeekAheadW(theLDB, $001C));

      //battle layout
      if not PeekAhead(theLDB, $1D) then
         raise EParseMessage.create('Battle Layout section of RPG_RT.LDB not found!');
      converter.read(theLDB); // read the length statement
      FBattleLayout := TBattleLayout.Create(theLDB);

      skipSec($1E, theLDB); //class section has already been read

      assert(PeekAheadW(theLDB, $001F));

      //Animations section
      if not PeekAhead(theLDB, $20) then
         raise EParseMessage.create('Animation section of RPG_RT.LDB not found!');
      converter.read(theLDB); // read the length statement
      converter.read(theLDB); // this one is the number of items
      setLength(FAnim2, converter.getData + 1);
      //0-index array fix
      FAnim2[0] := nil;
      for i := 1 to high(FAnim2) do
         FAnim2[i] := TBattleAnim2.Create(theLDB, i);
   end;
   assert(theLDB.position = theLDB.Size);
except
   on E: EParseMessage do
   begin
      msgBox(E.message, 'TLcfDataBase.Create says:', MB_OK);
      raise EMessageAbort.Create
   end
end // end of TRY block
end;

destructor TLcfDataBase.Destroy;
var
  I: Integer;
begin
   FVocabDict.Free;
   for I := 0 to high(FHeroes) do
      FHeroes[i].Free;
   for I := 0 to high(FItem) do
      FItem[i].Free;
   for I := 0 to high(FSkill) do
      FSkill[i].Free;
   for I := 0 to high(FCondition) do
      FCondition[i].Free;
   for I := 0 to high(FBattleAnim) do
      FBattleAnim[i].free;
   for I := 0 to high(FAttribute) do
      FAttribute[i].free;
   for I := 0 to high(FTerrain) do
      FTerrain[i].free;
   for I := 0 to high(FClass) do
      FClass[i].free;
   for I := 0 to high(chipSet) do
      chipSet[i].free;
   for i := 0 to high(FMonster) do
      FMonster[i].Free;
   for i := 0 to high(FMParty) do
      FMParty[i].Free;
   for i := 0 to high(FAnim2) do
      FAnim2[i].Free;
   FBattleLayout.Free;
   FVariables.Free;
   FSwitches.Free;
   FSystemData.free;
   FreeAndNil(FGlobalEvents);
   inherited;
end;

function TLcfDataBase.getSkill(x: word): TSkill;
begin
   result := FSkill[x];
end;

function TLcfDataBase.getSkillCount: word;
begin
   result := high(FSkill);
end;

function TLcfDataBase.getTerrain(x: word): TTerrainInfo;
begin
   result := FTerrain[x];
end;

function TLcfDataBase.heroCount: word;
begin
   result := high(FHeroes);
end;

function TLcfDataBase.getMaxChipsets: word;
begin
   result := chipSets;
end;

function TLcfDataBase.getMonster(x: word): TMonster;
begin
   result := FMonster[x];
end;

function TLcfDataBase.getMonsterParties: word;
begin
   result := high(FMParty);
end;

function TLcfDataBase.getMonsters: word;
begin
   result := high(FMonster);
end;

function TLcfDataBase.getAnim(x: word): TBattleAnim;
begin
   if IsBetween(x, 1, high(FBattleAnim)) then
      result := FBattleAnim[x]
   else result := nil;
end;

function TLcfDataBase.getAttribute(x: word): TAttribute;
begin
   result := FAttribute[x];
end;

function TLcfDataBase.getChipset(id: word): TChipSet;
begin
   result := chipSet[id];
end;

function TLcfDataBase.getClass(x: word): TRm2CharClass;
begin
   result := FClass[x];
end;

function TLcfDataBase.getClassCount: word;
begin
   result := high(FClass);
end;

function TLcfDataBase.getCommand(x: word): TBattleCommand;
begin
   result := FBattleLayout.FCommands[x];
end;

function TLcfDataBase.getCommands: word;
begin
   result := high(FBattleLayout.FCommands);
end;

function TLcfDataBase.getCondition(x: word): TCondition;
begin
   result := FCondition[x];
end;

function TLcfDataBase.getConditionCount: word;
begin
   result := high(FCondition);
end;

function TLcfDataBase.getHero(x: word): THeroRecord;
begin
   result := FHeroes[x];
end;

function TLcfDataBase.getItem(x: word): TItem;
begin
   result := FItem[x];
end;

function TLcfDataBase.seekBlankChipset: word;
var
   i: word;
begin
   result := 0;
   for i := 1 to chipSets do
   begin
      if (chipSet[i].name = '') and chipset[i].incomplete then
         if chipSet[i].empty then
            result := i;
      if result > 0 then
         break;
   end;
   if result = 0 then
      result := chipsets + 1;
end;

{ TVarSection }
constructor TVarSection.create(input: TStream; size: word);
var
  I: Integer;
  dummy: string;
  converter: intX80;
begin
   FLength := size;
   setLength(FVarNames, size + 1);
   for I := 1 to size do
   begin
      converter := TBerConverter.Create(input);
      if converter.getData <> i then
         raise EParseMessage.create('Switch value x' + intToHex(i, 2) + ' not found!');
      dummy := unicodeString(getStrSec(1, input, fillInSwitchStr));
      while pos('=', dummy) <> 0 do
         dummy[pos('=', dummy)] := ':';
      FVarNames[i] := ansiString(dummy);
      if not peekAhead(input, 0) then
         raise EParseMessage.create('Exceptional case found at LDB string x' + intToHex(i, 2) + '!');
   end;
end;

function TVarSection.getName(x: word): ansiString;
begin
   result := FVarNames[x];
end;

procedure TVarSection.setName(x: word; data: ansiString);
begin
   FVarNames[x] := data;
end;

{ TSwitchSection }
constructor TSwitchSection.create(input: TStream; size: word);
var
  I: Integer;
  dummy: string;
  converter: intX80;
begin
   FLength := size;
   setLength(FSwitchNames, size + 1);
   for I := 1 to size do
   begin
      converter := TBerConverter.Create(input);
      if converter.getData <> i then
         raise EParseMessage.create('Switch value x' + intToHex(i, 2) + ' not found!');
      dummy := unicodeString(getStrSec(1, input, fillInSwitchStr));
      while pos('=', dummy) <> 0 do
         dummy[pos('=', dummy)] := ':';
      FSwitchNames[i] := ansiString(dummy);
      if not peekAhead(input, 0) then
         raise EParseMessage.create('Exceptional case found at LDB string x' + intToHex(i, 2) + '!');
   end;
end;

function TSwitchSection.getName(x: word): ansiString;
begin
   result := FSwitchNames[x];
end;

procedure TSwitchSection.setName(x: word; data: ansiString);
begin
   FSwitchNames[x] := data;
end;

{ TSystemRecord }

constructor TSystemRecord.Create(input: TStream);
var
   i: byte;
begin
   if GProjectFormat = pf_2k3 then
      assert(getnumSec($0A, input, fillInZeroInt) = 2003);
   FVehicleGraphic[vh_boat] := getStrSec($0B, input, fillInBlankStr);
   FVehicleGraphic[vh_ship] := getStrSec($0C, input, fillInBlankStr);
   FVehicleGraphic[vh_airship] := getStrSec($0D, input, fillInBlankStr);
   FVehicleIndex[vh_boat] := getNumSec($0E, input, fillInZeroInt);
   FVehicleIndex[vh_ship] := getNumSec($0F, input, fillInZeroInt);
   FVehicleIndex[vh_airship] := getNumSec($10, input, fillInZeroInt);
   FTitleScreen := getStrSec($11, input, fillInBlankStr);
   FGameOverScreen := getStrSec($12, input, fillInBlankStr);

   FSysGraphic := getStrSec($13, input, fillInBlankStr);
   if GProjectFormat = pf_2k3 then
      FBattleSysGraphic := getstrSec($14, input, fillInBlankStr);
   FStartingHeroes := getNumSec($15, input, fillInSysRecordInt);
   getArraySec($16, input, FStartingHero);
   if GProjectFormat = pf_2k3 then
   begin
      setLength(FCommands, GetNumSec($1A, input, fillInZeroInt));
      if length(FCommands) > 0 then
         getArraySec($1B, input, FCommands[0])
      else skipSec($1B, input);
   end;
   FBgm[bgmTitle] := TRmMusic.Create($1F, input);
   FBgm[bgmBattle] := TRmMusic.Create($20, input);
   FBgm[bgmBossBattle] := TRmMusic.assign(FBgm[bgmBattle]);
   FBgm[bgmVictory] := TRmMusic.Create($21, input);
   FBgm[bgmInn] := TRmMusic.Create($22, input);
   FBgm[bgmBoat] := TRmMusic.Create($23, input);
   FBgm[bgmShip] := TRmMusic.Create($24, input);
   FBgm[bgmAirship] := TRmMusic.Create($25, input);
   FBgm[bgmGameOver] := TRmMusic.Create($26, input);
   for i := 0 to ord(sfxItemUsed) do
      FSfx[TSfxTypes(i)] := TRmSound.Create($29 + i, input);
   for i := 0 to ord(trnBattleEndShow) do
      FTransition[TTransitionTypes(i)] := getNumSec($3D + i, input, fillInZeroInt);
   FWallpaperStretch := getChboxSec($47, input, fillInZeroInt);

   FWhichFont := getNumSec($48, input, fillInZeroInt);
   FLegacy := TLegacyDict.Create;
   FLegacy.Add($51, GetStrSec($51, input, fillInBlankStr));
   FLegacy.Add($52, GetStrSec($52, input, fillInBlankStr));
   FEditorBattleBG := GetStrSec($54, input, fillInBlankStr);
   FLegacy.Add($55, GetStrSec($55, input, fillInBlankStr));
   FLegacy.Add($5B, GetStrSec($5B, input, fillInBlankStr));

   if GProjectFormat = pf_2k3 then
   begin
      for i := $5E to $62 do
         FLegacy.Add(i, GetStrSec(i, input, fillInBlankStr));
      FUsesFrame := GetChboxSec($63, input, fillInZeroInt);
      FFrame := GetStrSec($64, input, fillInBlankStr);
      FReverseGraphics := GetChboxSec($65, input, fillInZeroInt);
   end;
   if not peekAhead(input, 0) then
      raise EParseMessage.create('Exceptional case found at LDB system section x' + intToHex(getNext(input), 2) + '!');
end;

destructor TSystemRecord.Destroy;
var
   i: TBgmTypes;
   j: TSfxTypes;
begin
   FLegacy.Free;
   for i := low(TBgmTypes) to high(TBgmTypes) do
      FBgm[i].Free;
   for j := low(TSfxTypes) to high(TSfxTypes) do
      FSfx[j].Free;
   inherited;
end;

function TSystemRecord.getBgm(which: TBgmTypes): TRmMusic;
begin
   result := FBgm[which];
end;

function TSystemRecord.getSfx(which: TSfxTypes): TRmSound;
begin
   result := FSfx[which];
end;

function TSystemRecord.getStartingHero(which: word): word;
begin
   assert(which in [1..4]);
   result := FStartingHero[which];
end;

function TSystemRecord.getVehicleGraphic(which: TVehicleSet): ansiString;
begin
   result := FVehicleGraphic[which];
end;

function TSystemRecord.getVehicleIndex(which: TVehicleSet): byte;
begin
   result := FVehicleIndex[which];
end;

procedure TSystemRecord.setBgm(which: TBgmTypes; const Value: TRmMusic);
begin
   FBgm[which] := value;
end;

procedure TSystemRecord.setSfx(which: TSfxTypes; const Value: TRmSound);
begin
   FSfx[which] := value;
end;

function TSystemRecord.getTransition(which: TTransitionTypes): byte;
begin
   result := FTransition[which];
end;

procedure TSystemRecord.setTransition(which: TTransitionTypes; const Value: byte);
begin
   FTransition[which] := value;
end;

procedure TSystemRecord.setVehicleGraphic(which: TVehicleSet; const Value: ansiString);
begin
   FVehicleGraphic[which] := value;
end;

procedure TSystemRecord.setVehicleIndex(which: TVehicleSet; const Value: byte);
begin
   FVehicleIndex[which] := value;
end;

{ TTerrainInfo }

constructor TTerrainInfo.Create(input: TStream; const id: word);
var
   converter: intX80;
   I: Integer;
begin
   inherited create;
   converter := TBerConverter.Create(input);
   assert(converter.getData = id, 'Terrain record' + intToStr(id) + ' of RPG_RT.LDB not found!');
   FLegacy := TLegacyDict.Create;

   FName := getStrSec(1, input, fillInBlankStr);
   FDamage := getNumSec(2, input, fillInZeroInt);
   FEncounterMultiplier := getNumSec(3, input, fillInTerrainInt);
   FBattleBg := getStrSec(4, input, fillInBlankStr);
   FVehiclePass[vh_boat] := getChboxSec(5, input, fillInZeroInt);
   FVehiclePass[vh_ship] := getChboxSec(6, input, fillInZeroInt);
   FVehiclePass[vh_airship] := getChboxSec(7, input, fillInTerrainInt);
   FAirshipLanding := getChboxSec(9, input, fillInTerrainInt);
   FConcealment := TConcealmentFactor(getNumSec($B, input, fillInZeroInt));
   if GProjectFormat = pf_2k3 then
   begin
      FSoundEffect := TRmSound.Create($F, input);
      for I := $10 to $14 do
         FLegacy.Add(i, GetStrSec(i, input, fillInBlankStr));
      FFrame := GetStrSec($15, input, fillInBlankStr);
      for I := $16 to $30 do
         FLegacy.Add(i, GetStrSec(i, input, fillInBlankStr));
   end;
   assert(peekAhead(input, 0));
end;

destructor TTerrainInfo.Destroy;
begin
   FLegacy.Free;
   FSoundEffect.Free;
   inherited;
end;

function TTerrainInfo.vehicleCanPass(which: TVehicleSet): boolean;
begin
   result := FVehiclePass[which];
end;

{ TAttribute }

constructor TAttribute.Create(theLDB: TStream; const id: word);
var
   converter: intX80;
begin
   inherited create;
   converter := TBerConverter.Create(theLDB);
   if converter.getData <> id then
      raise EParseMessage.create('Attribute record' + intToStr(id) + ' of RPG_RT.LDB not found!');
   FName := getStrSec(1, theLDB, fillInBlankStr);
   FWeaponRestrict := not getChboxSec(2, theLDB, fillInZeroInt);
   FRateA := getNumSec($B, theLDB, fillInAttribInt);
   FRateB := getNumSec($C, theLDB, fillInAttribInt);
   FRateC := getNumSec($D, theLDB, fillInAttribInt);
   FRateD := getNumSec($E, theLDB, fillInAttribInt);
   FRateE := getNumSec($F, theLDB, fillInZeroInt);
   assert(peekAhead(theLDB, 0));
end;

{$WARN NO_RETVAL OFF}
function TAttribute.getRate(value: byte): smallint;
begin
   case value of
      1: result := FRateA;
      2: result := FRateB;
      3: result := FRateC;
      4: result := FRateD;
      5: result := FRateE;
      else assert(false);
   end;
end;
{$WARN USE_BEFORE_DEF ON}

{ TBattleCommand }

constructor TBattleCommand.Create(input: TStream; const id: word);
begin
   inherited Create;
   assert(peekAhead(input, id));
   FName := getStrSec(1, input, fillInBlankStr);
   FStyle := TCommandStyle(getNumSec(2, input, fillInZeroInt));
   assert(peekAhead(input, 0));
end;

{ TBattleLayout }

constructor TBattleLayout.Create(input: TStream);
var
   converter: intX80;
   i: integer;
begin
   inherited Create;
   FAutoLineup := getChboxSec(2, input, fillInZeroInt);
   FDeathEvent := getChboxSec(4, input, fillInZeroInt);
   skipSec(6, input); //06 can be skipped; purely visual element
   FBattleStyle := getNumSec(7, input, fillInZeroInt);
   assert(getNumSec(9, input, fillInZeroInt) = 0); //assert 09 = 0; not sure what it's for
   assert(peekAhead(input, $0A)); //battle commands section
   converter := TBerConverter.Create(input); //discard length statement
   converter.read(input);
   SetLength(FCommands, converter.getData + 1);
   FCommands[0] := nil;
   for I := 1 to high(FCommands) do
      FCommands[i] := TBattleCommand.Create(input, i);
   FUsesDeathEventHandler := getChboxSec($0F, input, fillInZeroInt); 
   FDeathEventHandler := getNumSec($10, input, fillInZeroInt);
   FWindowSize := getNumSec($14, input, fillInZeroInt); //0: large; 1: small
   FWindowTrans := getChboxSec($18, input, fillInZeroInt);
   FTeleportOnDeath := getChboxSec($19, input, fillInZeroInt);
   FescapeMap := getNumSec($1A, input, fillInZeroInt);
   FEscapePoint.x := getNumSec($1B, input, fillInZeroInt);
   FEscapePoint.y := getNumSec($1C, input, fillInZeroInt);
   FUnk1D := getNumSec($1D, input, fillInZeroInt);
   assert(peekAhead(input, 0));
end;

destructor TBattleLayout.Destroy;
var
   i: integer;
begin
   for i := 0 to high(FCommands) do
      FCommands[i].Free;
   inherited;
end;

{Classless}

function globalEventBlock: TObject;
begin
   result := FGlobalEvents;
end;

procedure fillInSwitchStr(const expected: byte; out theResult: ansiString);
begin
   msgBox ('No case implemented for x' + IntToHex(expected, 2) + '!', 'fillInSwitchStr says:', MB_OK);
   raise EMessageAbort.Create
end;

procedure fillInAttribInt(const expected: byte; out theResult: integer);
begin
   case expected of
      $B: theResult := 150;
      $C: theResult := 125;
      $D: theResult := 100;
      $E: theResult := 50;
   else
   begin
      msgBox ('No case implemented for x' + IntToHex(expected, 2) + '!', 'fillInLdbStr says:', MB_OK);
      raise EMessageAbort.Create
   end;
   end;
end;

procedure fillInTerrainInt(const expected: byte; out theResult: integer);
begin
   case expected of
      3: theResult := 100;
      7,9: theResult := 1;
   else
   begin
      msgBox ('No case implemented for x' + IntToHex(expected, 2) + '!', 'fillInLdbStr says:', MB_OK);
      raise EMessageAbort.Create
   end;
   end;
end;

procedure fillInSysRecordInt(const expected: byte; out theResult: integer);
begin
   case expected of
      $15: theResult := 1;
   else
   begin
      msgBox ('No case implemented for x' + IntToHex(expected, 2) + '!', 'fillInLdbStr says:', MB_OK);
      raise EMessageAbort.Create
   end;
   end;
end;

initialization
begin
   FGlobalEvents := nil;
end;

finalization
begin
   FGlobalEvents.free;
end;

end.
