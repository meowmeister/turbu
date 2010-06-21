unit item_data;
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

uses windows, classes, sysUtils, //windows libs
     BER, commons, fileIO;

type
   TItemType = (commonItem, weaponItem, shieldItem, armorItem, helmetItem,
               accessoryItem, medicineItem, bookItem, materialItem, uniqueItem,
               switchItem);
   TWeaponAnimType = (wa_weapon, wa_battleAnim);
   TMovementMode = (mm_none, mm_stepForward, mm_jumpTo, mm_walkTo);

   TItemAnimData = class(TObject)
   private
       FAnimType: TWeaponAnimType; //3
       FWhichWeapon: smallint;     //4
       FMovementMode: TMovementMode; //5
       FAfterimage: boolean; //6
       FAttackNum: byte; //7
       FRanged: boolean; //8
       FRangedProjectile: smallint; //9
       FRangedSpeed: byte; //C
       FBattleAnim: word; //D
   public
      constructor Create(theLDB: TStream; const id: word);

      property animType: TWeaponAnimType read FAnimType;
      property whichWeapon: smallint read FWhichWeapon;
      property moveMode: TMovementMode read FMovementMode;
      property afterimage: boolean read FAfterimage;
      property attackNum: byte read FAttackNum;
      property ranged: boolean read FRanged;
      property rangedProjectile: smallint read FRangedProjectile;
      property rangedSpeed: byte read FRangedSpeed;
      property battleAnim: word read FBattleAnim;
   end;

   TItem = class(TObject)
   private
      FId: word;
      FIncomplete: boolean;
      FName: ansiString;
      FDesc: ansiString;
      FItemType: TItemType;
      FPrice: integer;
      FUses: smallint;
      FAttackModify: smallint;
      FDefenseModify: smallint;
      FMindModify: smallint;
      FSpeedModify: smallint;
      FTwoHanded: boolean;
      FMPCost: word;
      FToHit: byte;
      FCritChance: byte;
      FBattleAnim: word;
      FPreemptive: boolean;
      FAttackTwice: boolean;
      FAreaHit: boolean;
      FIgnoreEvasion: boolean;
      FPreventCrits: boolean;
      FBoostEvade: boolean;
      FHalfMP: boolean;
      FNoTerrainDamage: boolean;
      FAreaMedicine: boolean;
      FHPPercent: shortint;
      FHPHeal: smallint;
      FMPPercent: shortint;
      FMPHeal: smallint;
      FOutOfBattleOnly: boolean;
      FDeadHeroesOnly: boolean;
      FPermHPGain: smallint;
      FPermMPGain: smallint;
      FPermAttackGain: smallint;
      FPermDefenseGain: smallint;
      FPermMindGain: smallint;
      FPermSpeedGain: smallint;
      FDisplaySkillMessage: boolean;
      FSkillToLearn: word;
      FSwitch: word;
      FOnField: boolean;
      FInBattle: boolean;
      FUsableBy: array of boolean;
      FConditions: array of boolean;
      FAttributes: array of boolean;
      FConditionInflictChance: byte;
      //2003 specific:
      FCursed: boolean;
      FUsableByClass: array of boolean;
      FAnimData: array of TItemAnimData;
      FInvokeSkill: boolean;

      function getStat(which: byte): word;
      function getCondition(x: word): boolean;
      function isUsableBy(id: word): boolean;
      function isUsableByClass(x: word): boolean;
      function getAnimData(x: word): TItemAnimData;
      function getAttribute(x: word): boolean;
      function getConditions: word;
   public
      constructor Create(theLDB: TStream; const id: word; parent: TObject);
      destructor Destroy; override;
      procedure reviseConditions(size: integer);

      property incomplete: boolean read FIncomplete;
      property id: word read FId;
      property name: ansiString read FName;
      property desc: ansiString read FDesc;
      property usesLeft: smallint read FUses;
      property itemType: TItemType read FItemType;
      property cost: integer read FPrice;
      property attack: smallint read FAttackModify;
      property defense: smallint read FDefenseModify;
      property mind: smallint read FMindModify;
      property speed: smallint read FSpeedModify;
      property stat[which: byte]: word read getStat;
      property twoHanded: boolean read FTwoHanded;
      property ignoreEvasion: boolean read FIgnoreEvasion;
      property attackTwice: boolean read FAttackTwice;
      property areaHit: boolean read FAreaHit;
      property battleAnim: word read FBattleAnim;
      property boostEvade: boolean read FBoostEvade;
      property preventCrits: boolean read FPreventCrits;
      property mpCost: word read FMPCost;
      property conditionChance: byte read FConditionInflictChance;
      property usableBy[x: word]: boolean read isUsableBy;
      property areaMedicine: boolean read FAreaMedicine;
      property hpPercent: shortint read FHpPercent;
      property hpHeal: smallint read FHpHeal;
      property mpPercent: shortint read FMpPercent;
      property mpHeal: smallint read FMpHeal;
      property outOfBattle: boolean read FOutOfBattleOnly;
      property deadOnly: boolean read FDeadHeroesOnly;
      property permHP: smallint read FPermHPGain;
      property permMp: smallint read FPermMPGain;
      property permAttack: smallint read FPermAttackGain;
      property permDefense: smallint read FPermDefenseGain;
      property permMind: smallint read FPermMindGain;
      property permSpeed: smallint read FPermSpeedGain;
      property customSkillMessage: boolean read FDisplaySkillMessage;
      property switch: word read FSwitch;
      property skill: word read FSkillToLearn;
      property onField: boolean read FOnField;
      property inBattle: boolean read FInBattle;
      property condition[x: word]: boolean read getCondition;
      property conditions: word read getConditions;
      property attribute[x: word]: boolean read getAttribute;
      //2003 specific:
      property cursed: boolean read FCursed;
      property usableByClass[x: word]: boolean read isUsableByClass;
      property animData[x: word]: TItemAnimData read getAnimData;
   end;


implementation
uses
   LDB, formats;

procedure fillInItemInt(const expected: byte; out theResult: integer); forward;

{ TItem }

constructor TItem.Create(theLDB: TStream; const id: word; parent: TObject);
var
   dummy: word;
   converter: intx80;
   I: integer;
   stringData: ansiString;
   stringStream: TStringStream;
   incompleteSection: boolean;
begin
   inherited Create;
   assert(parent is TLcfDataBase);
try
with theLDB do
begin
   FIncomplete := false;
   converter := TBerConverter.Create(theLDB);
   dummy := converter.getData;
   if dummy <> id then
      raise EParseMessage.create('Item section ' + intToStr(id) + ' of RPG_RT.LDB not found!');
   FId := id;
   if peekAhead(theLDB, 0) = false then //blank item records just contain an x00 and nothing else
   begin
      FName := getStrSec(1, theLDB, fillInBlankStr);
      if peekAhead(theLDB, 0) then
      begin
         FIncomplete := true;
         exit
      end;
      FDesc := getStrSec(2, theLDB, fillInBlankStr);
      if peekAhead(theLDB, 0) then
      begin
         FIncomplete := true;
         exit
      end;
      FItemType := TItemType(getNumSec(3, theLDB, fillInItemInt));
      FPrice := getNumSec(5, theLDB, fillInZeroInt);
      FUses := getNumSec(6, theLDB, fillInItemInt);
      FAttackModify := getNumSec($B, theLDB, fillInZeroInt);
      FDefenseModify := getNumSec($C, theLDB, fillInZeroInt);
      FMindModify := getNumSec($D, theLDB, fillInZeroInt);
      FSpeedModify := getNumSec($E, theLDB, fillInZeroInt);
      FTwoHanded := getChboxSec($F, theLDB, fillInZeroInt);
      FMPCost := getNumSec($10, theLDB, fillInZeroInt);
      FToHit := getNumSec($11, theLDB, fillInZeroInt);
      FCritChance := getNumSec($12, theLDB, fillInZeroInt);
      FBattleAnim := getNumSec($14, theLDB, fillInItemInt);
      FPreemptive := getChboxSec($15, theLDB, fillInZeroInt);
      FAttackTwice := getChboxSec($16, theLDB, fillInZeroInt);
      FAreaHit := getChboxSec($17, theLDB, fillInZeroInt);
      FIgnoreEvasion := getChboxSec($18, theLDB, fillInZeroInt);
      FPreventCrits := getChboxSec($19, theLDB, fillInZeroInt);
      FBoostEvade := getChboxSec($1A, theLDB, fillInZeroInt);
      FHalfMP := getChboxSec($1B, theLDB, fillInZeroInt);
      FNoTerrainDamage := getChboxSec($1C, theLDB, fillInZeroInt);
      FCursed := getChboxSec($1D, theLDB, fillInZeroInt);
      FAreaMedicine := getChboxSec($1F, theLDB, fillInZeroInt);
      FHpPercent := getNumSec($20, theLDB, fillInZeroInt);
      FHpHeal := getNumSec($21, theLDB, fillInZeroInt);
      FMpPercent := getNumSec($22, theLDB, fillInZeroInt);
      FMpHeal := getNumSec($23, theLDB, fillInZeroInt);
      FOutOfBattleOnly := getChboxSec($25, theLDB, fillInZeroInt);
      FDeadHeroesOnly := getChboxSec($26, theLDB, fillInZeroInt);
      FPermHPGain := getNumSec($29, theLDB, fillInZeroInt);
      FPermMpGain := getNumSec($2A, theLDB, fillInZeroInt);
      FPermAttackGain := getNumSec($2B, theLDB, fillInZeroInt);
      FPermDefenseGain := getNumSec($2C, theLDB, fillInZeroInt);
      FPermMindGain := getNumSec($2D, theLDB, fillInZeroInt);
      FPermSpeedGain := getNumSec($2E, theLDB, fillInZeroInt);
      FDisplaySkillMessage := getChboxSec($33, theLDB, fillInZeroInt);

      FSkillToLearn := getNumSec($35, theLDB, fillInZeroInt);
      FSwitch := getNumSec($37, theLDB, fillInZeroInt);
      FOnField := getChboxSec($39, theLDB, fillInZeroInt);
      FInBattle := getChboxSec($3A, theLDB, fillInZeroInt);

      setLength(FUsableBy, getNumSec($3D, theLDB, fillInZeroInt) + 1);
      if length(FUsableBy) = 1 then
         setLength(FUsableBy, TLcfDataBase(parent).heroes + 1);
      dummy := getArraySec($3E, theLDB, FUsableBy[1]);
      if dummy = 0 then
         for I := 1 to high(FUsableBy) do
            FUsableBy[i] := true
         //end for
      //end if
      else assert(dummy = high(FUsableBy));

      setLength(FConditions, getNumSec($3F, theLDB, fillInZeroInt) + 1);
{      if length(FConditions) = 1 then
         setLength(FConditions, TLcfDataBase(parent).conditions + 1);}
      if length(FConditions) > 1 then
         dummy := getArraySec($40, theLDB, FConditions[1])
      else skipSec($40, theLDB);

      setLength(FAttributes, getNumSec($41, theLDB, fillInZeroInt) + 1);
{      if length(FAttributes) = 1 then
         setLength(FAttributes, TLcfDataBase(parent).conditions + 1);}
      if length(Fattributes) > 1 then
         dummy := getArraySec($42, theLDB, Fattributes[1])
      else skipSec($42, theLDB);

      FConditionInflictChance := getNumSec($43, theLDB, fillInZeroInt);

      if GProjectFormat = pf_2k3 then
      begin
         for i := $44 to $45 do
            skipSec(i, theLDB);

         stringData := getStrSec($46, theLDB, fillInBlankStr);
         if stringData <> '' then
         begin
            setLength(FAnimData, byte(stringData[1]) + 1);
            Delete(stringData, 1, 1);
            stringStream := TStringStream.Create(stringData);
            try
               for I := 1 to high(FAnimData) do
                  FAnimData[i] := TItemAnimData.Create(stringStream, i);
            finally
               stringStream.free;
            end;
         end;
         FInvokeSkill := getChboxSec($47, theLDB, fillInZeroInt);
         incompleteSection := getNumSec($48, theLDB, fillInZeroInt) < TLcfDataBase(parent).charClasses;
         setLength(FUsableByClass, TLcfDataBase(parent).charClasses + 1);
         dummy := getArraySec($49, theLDB, FUsableByClass[0]);
         if incompleteSection then
            for I := dummy + 1 to high(FUsableByClass) do
               FUsableByClass[i] := true;

         for i := $4A to $4C do
            skipSec(i, theLDB);
         //end FOR
      end;
      Read(dummy, 1);
      if dummy and $FF <> 0 then
         raise EParseMessage.create('Item section ' + intToStr(id) + ' final 0 not found. Position: 0x' + intToHex(theLDB.Position, 2));
      //end if
   end;
end; // end of WITH block
except
   on E: EParseMessage do
   begin
      msgBox(E.message, 'TChipSet.Create says:', MB_OK);
      raise EMessageAbort.Create
   end
end; // end of TRY block
end;

destructor TItem.Destroy;
var
   i: integer;
begin
   for i := 0 to high(FAnimData) do
      FAnimData[i].free;
   inherited;
end;

function TItem.getAnimData(x: word): TItemAnimData;
begin
   if length(FAnimData) > 1 then
      result := FAnimData[x]
   else result := FAnimData[1];
end;

function TItem.getAttribute(x: word): boolean;
begin
   if x > high(FAttributes) then
      result := false
   else result := FAttributes[x];
end;

function TItem.getCondition(x: word): boolean;
begin
   if x > high(FConditions) then
      result := false
   else result := FConditions[x];
end;

function TItem.getConditions: word;
begin
   result := High(FConditions);
end;

function TItem.getStat(which: byte): word;
begin
   case which of
      1: result := FAttackModify;
      2: result := FDefenseModify;
      3: result := FMindModify;
      4: result := FSpeedModify;
      else raise EFatalError.create('Tried to get an invalid equipment stat!');
   end;
end;

function TItem.isUsableBy(id: word): boolean;
begin
   if id > high(FUsableBy) then
      result := true
   else result := FUsableBy[id];
end;

function TItem.isUsableByClass(x: word): boolean;
begin
   result := FUsableByClass[x];
end;

procedure TItem.reviseConditions(size: integer);
var i, dummy: word;
begin
   if high(FConditions) < size then
   begin
      dummy := high(FConditions);
      setLength(FConditions, size + 1);
      for I := dummy + 1 to high(FConditions) do
         FConditions[i] := false;
      //end FOR
   end;
end;

{ TItemAnimData }

constructor TItemAnimData.Create(theLDB: TStream; const id: word);
var
   dummy: word;
   converter: intx80;
begin
   inherited Create;
   converter := TBerConverter.Create(theLDB);
   dummy := converter.getData;
   if dummy <> id then
      raise EParseMessage.create('ItemAnimData section ' + intToStr(id) + ' not found!');
   FAnimType := TWeaponAnimType(getNumSec(3, theLDB, fillInZeroInt));
   FWhichWeapon := getNumSec(4, theLDB, fillInZeroInt);
   FMovementMode := TMovementMode(getNumSec(5, theLDB, fillInZeroInt));
   FAfterimage := getChboxSec(6, theLDB, fillInZeroInt);
   FAttackNum := getNumSec(7, theLDB, fillInZeroInt);
   FRanged := getChboxSec(8, theLDB, fillInZeroInt);
   FRangedProjectile := getNumSec(9, theLDB, fillInZeroInt);
   FRangedSPeed := getNumSec($C, theLDB, fillInZeroInt);
   FBattleAnim := getNumSec($D, theLDB, fillInZeroInt);
   if not peekAhead(theLDB, 0) then
      raise EParseMessage.create('ItemAnimData section ' + intToStr(id) + ' final 0 not found. Position: 0x' + intToHex(theLDB.Position, 2));
end;

{ Classless }

procedure fillInItemInt(const expected: byte; out theResult: integer);
begin
   case expected of
      6, $14: theResult := 1;
   else
   begin
      msgBox ('No case implemented for x' + IntToHex(expected, 2) + '!', 'fillInLdbStr says:', MB_OK);
      raise EMessageAbort.Create
   end;
   end;
end;

end.
