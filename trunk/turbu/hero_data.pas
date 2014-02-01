unit hero_data;
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
     BER, commons, fileIO,
     turbu_defs; //turbu libs

type
   TGameStat = (stat_HP, stat_MP, stat_str, stat_def, stat_mind, stat_agi);

   THeroSkillRecord = class(TObject)
   private
      FLevel: byte;
      FSkill: word;
   public
      constructor create(input: TStream; const id: byte);

      property id: word read FSkill;
      property level: byte read FLevel;
   end;

   THeroRecord = class
   private
      FID: byte;
      FName: ansiString;
      FClass: ansiString;
      FGraphic: ansiString;
      FGraphicIndex: byte;
      FTransparent: boolean;
      FStartLevel: smallint;
      FMaxLevel: smallint;
      FCanCrit: boolean;
      FCritRate: byte;
      FFaceName: ansiString;
      FFaceNum: byte;
      FDualWield: boolean;
      FStaticEq: boolean;
      FComputerControlled: boolean;
      FStrongDefense: boolean;
      FCurveSec: array of word;
      FExpStandard: word;
      FExpAddition: word;
      FExpCorrection: word;
      FInitialEQ: array[1..5] of word;
      FUnarmedAnim: word;
      FClassNum: integer;
      FBattleX: integer;
      FBattleY: integer;
      FBattleChar: word;
      FSkillSection: array of THeroSkillRecord;
      FMagicRenamed: boolean;
      FSkillCategoryName: ansiString;
      FConditionModifiers: array of byte;
      FDtypeModifiers: array of byte;
      FBattleCommands: array[1..7] of integer;

      function getInitialEq(x: byte): word;
      function getStatCurve(whichStat: TGameStat; index: byte): word;
      function getCModifierCount: word;
      function getConditionModifier(x: word): byte;
      function getSkill(x: word): THeroSkillRecord;
      function getSkillCount: word;
      function getCurveSec: pointer;
      function getDModifierCount: word;
      function getDtypeModifier(x: word): byte;
      function GetBattlePos: TPoint;
   public
      constructor create(input: TStream; const id: byte; parent: TObject);
      destructor Destroy; override;
      procedure assign(const data: THeroRecord);
      procedure reviseConditions(size: integer);
      function StatSlice(low, high: integer): TIntArray;

      property filename: ansiString read FGraphic;
      property name: ansiString read FName write FName;
      property charClass: ansiString read FClass write FClass;
      property classNum: integer read FClassNum write FClassNum;
      property sprite: ansiString read FGraphic write FGraphic;
      property spriteIndex: byte read FGraphicIndex write FGraphicIndex;
      property transparent: boolean read FTransparent write FTransparent;
      property startLevel: smallint read FStartLevel write FStartLevel;
      property maxLevel: Smallint read FMaxLevel write FMaxLevel;
      property canCrit: boolean read FCanCrit write FCanCrit;
      property critRate: byte read FCritRate write FCritRate;
      property dualWield: boolean read FDualWield write FDualWield;
      property staticEq: boolean read FStaticEq write FStaticEq;
      property computerControlled: boolean read FComputerControlled write FComputerControlled;
      property strongDef: boolean read FStrongDefense write FStrongDefense;
      property portrait: ansiString read FFaceName write FFaceName;
      property portraitIndex: byte read FFaceNum write FFaceNum;
      property expStandard: word read FExpStandard write FExpStandard;
      property expAddition: word read FExpAddition write FExpAddition;
      property expCorrection: word read FExpCorrection write FExpCorrection;
      property skills: word read getSkillCount;
      property skill[x: word]: THeroSkillRecord read getSkill;
      property hasSkillName: boolean read FMagicRenamed;
      property skillName: ansiString read FSkillCategoryName;
      property conditionModifiers: word read getCModifierCount;
      property conditionModifier[x: word]: byte read getConditionModifier;
      property dtypeModifiers: word read getDModifierCount;
      property dtypeModifier[x: word]: byte read getDtypeModifier;
      property statCurve[whichStat: TGameStat; index: byte]: word read getStatCurve;
      property initialEq[x: byte]: word read getInitialEq;
      property unarmedAnim: word read FUnarmedAnim;
      property battleSprite: word read FBattleChar write FBattleChar;
      property battlePosition: TPoint read GetBattlePos;
      property id: byte read FID;
   end;

   TRm2CharClass = class(TObject)
   private
      FID: byte;
      FName: ansiString; //01
      FCurveSec: array of word; //1F
      FExpStandard: word; //29
      FExpAddition: word; //2A
      FExpCorrection: word; //2B
      FGraphicIndex: byte; //3E
      FSkillSection: array of THeroSkillRecord; //3F
      FConditionModifiers: array of byte;
      FDtypeModifiers: array of byte;
      FBattleCommands: array[1..7] of integer;
      FDualWield: boolean;
      FStaticEq: boolean;
      FComputerControlled: boolean;
      FStrongDefense: boolean;

      function getStatCurve(whichStat: TGameStat; index: byte): word;
      function getCModifierCount: word;
      function getConditionModifier(x: word): byte;
      function getSkill(x: word): THeroSkillRecord;
      function getSkillCount: word;
      function getCurveSec: pointer; inline;
      function getCommand(x: integer): integer; inline;
      function getDModifierCount: word;
      function getDtypeModifier(x: word): byte;
   public
      constructor create(input: TStream; const id: byte; parent: TObject);
      destructor Destroy; override;
      procedure assign(const data: TRm2CharClass);
      procedure reviseConditions(size: integer);

      property name: ansiString read FName write FName;
      property spriteIndex: byte read FGraphicIndex write FGraphicIndex;
      property dualWield: boolean read FDualWield write FDualWield;
      property staticEq: boolean read FStaticEq write FStaticEq;
      property computerControlled: boolean read FComputerControlled write FComputerControlled;
      property strongDef: boolean read FStrongDefense write FStrongDefense;
      property expStandard: word read FExpStandard write FExpStandard;
      property expAddition: word read FExpAddition write FExpAddition;
      property expCorrection: word read FExpCorrection write FExpCorrection;
      property skills: word read getSkillCount;
      property skill[x: word]: THeroSkillRecord read getSkill;
      property dtypeModifiers: word read getDModifierCount;
      property dtypeModifier[x: word]: byte read getDtypeModifier;
      property conditionModifiers: word read getCModifierCount;
      property conditionModifier[x: word]: byte read getConditionModifier;
      property statBlock: pointer read getCurveSec;
      property statCurve[whichStat: TGameStat; index: byte]: word read getStatCurve;
      property battleCommand[x: integer]: integer read getCommand;
      property id: byte read FID;
   end;

function calcExp(currentLevel: integer; const stdIncrease, addIncrease, correction: integer; dummy: integer): integer;

implementation
uses
   math,
   LDB, formats;

procedure fillInHeroInt(const expected: byte; out theResult: integer); forward;
procedure fillInHskillInt(const expected: byte; out theResult: integer); forward;

{ THeroData }

procedure THeroRecord.assign(const data: THeroRecord);
begin
assert(false);
end;

constructor THeroRecord.create(input: TStream; const id: byte; parent: TObject);
var
   converter: intX80;
   I: Integer;
begin
   assert(parent is TLcfDataBase);
   inherited create;
   FID := id;
   converter := TBerConverter.Create(input);
   if converter.getData <> id then
      raise EParseMessage.create('Hero record' + intToStr(id) + ' of RPG_RT.LDB not found!');
   FName := getStrSec(1, input, fillInBlankStr);
   FClass := getStrSec(2, input, fillInBlankStr);
   FGraphic := getStrSec(3, input, fillInBlankStr);
   FGraphicIndex := getNumSec(4, input, fillInHeroInt);
   FTransparent := getChboxSec(5, input, fillInHeroInt);
   FStartLevel := getNumSec(7, input, fillInHeroInt);
   FMaxLevel := getNumSec(8, input, fillInHeroInt);
   FCanCrit := getChboxSec(9, input, fillInHeroInt);
   FCritRate := getNumSec($0a, input, fillInHeroInt);
   FFacename := getStrSec($0f, input, fillInBlankStr);
   FFaceNum := getNumSec($10, input, fillInHeroInt);
   FDualWield := getChboxSec($15, input, fillInHeroInt);
   FStaticEq := getChboxSec($16, input, fillInHeroInt);
   FComputerControlled := getChboxSec($17, input, fillInHeroInt);
   FStrongDefense := getChboxSec($18, input, fillInHeroInt);
   case GProjectFormat of
      pf_2k: SetLength(FCurveSec, 600);
      pf_2k3: SetLength(FCurveSec, 1188);
      else assert(false, 'Bad project format variable!')
   end;
   assert(getArraySec($1f, input, FCurveSec[0]) = length(FCurveSec));
   FExpStandard := getNumSec($29, input, fillInHeroInt);
   FExpAddition := getNumSec($2a, input, fillInHeroInt);
   FExpCorrection := getNumSec($2b, input, fillInHeroInt);
   getArraySec($33, input, FInitialEQ);
   FUnarmedAnim := getNumSec($38, input, fillInHeroInt);
//Skill section
   if GProjectFormat = pf_2k3 then
   begin
      FClassNum := getNumSec($39, input, fillInZeroInt);
      FBattleX := getNumSec($3b, input, fillInZeroInt);
      FBattleY := getNumSec($3c, input, fillInZeroInt);
      FBattleChar := getNumSec($3e, input, fillInZeroInt);
   end;
   if peekAhead(input, $3f) then
   begin
      converter.read(input);
      converter.read(input);
      setLength(FSkillSection, converter.getData + 1);
      for I := 1 to high(FSkillSection) do
         FSkillSection[i] := THeroSkillRecord.create(input, i);
      //end FOR
   end
   else
      setLength(FSkillSection, 1);
   FSkillSection[0] := THeroSkillRecord.create(nil, 0);

   FMagicRenamed := getChboxSec($42, input, fillInHeroInt);
   FSkillCategoryName := getStrSec($43, input, fillInBlankStr);
   setLength(FConditionModifiers, getNumSec($47, input, fillInHeroInt) + 1);
   if length(FConditionModifiers) > 1 then
      getArraySec($48, input, FConditionModifiers[1])
   else
      getArraySec($48, input, FConditionModifiers[0]);
   //end if
   setLength(FDtypeModifiers, getNumSec($49, input, fillInHeroInt) + 1);
   if length(FDtypeModifiers) > 1 then
      getArraySec($4a, input, FDtypeModifiers[1])
   else
      getArraySec($4a, input, FDtypeModifiers[0]);
   if GProjectFormat = pf_2k3 then
      assert(getArraySec($50, input, FBattleCommands) = 28);
   assert(peekAhead(input, 0));
end;

destructor THeroRecord.Destroy;
var i: integer;
begin
   for i := low(FSkillSection) to high(FSkillSection) do
      FSkillSection[i].Free;
   inherited;
end;

function THeroRecord.GetBattlePos: TPoint;
begin
   result := point(FBattleX, FBattleY);
end;

function THeroRecord.getCModifierCount: word;
begin
   result := high(FConditionModifiers);
end;

function THeroRecord.getConditionModifier(x: word): byte;
begin
   if x <= high(FConditionModifiers) then
      result := FConditionModifiers[x]
   else result := 2;
end;

function THeroRecord.getCurveSec: pointer;
begin
   result := @FCurveSec[0];
end;

function THeroRecord.getInitialEq(x: byte): word;
begin
   result := FInitialEq[x];
end;

function THeroRecord.getDModifierCount: word;
begin
   result := high(FDtypeModifiers);
end;

function THeroRecord.getDtypeModifier(x: word): byte;
begin
   if x > high(FDtypeModifiers) then
      result := 3
   else result := FDtypeModifiers[x];
end;

function THeroRecord.getSkill(x: word): THeroSkillRecord;
begin
   if (x > 0) and (x <= high(FSkillSection)) then
      result := FSkillSection[x]
   else result := FSkillSection[0];
end;

function THeroRecord.getSkillCount: word;
begin
   result := high(FSkillSection);
end;

function THeroRecord.getStatCurve(whichStat: TGameStat; index: byte): word;
begin
   result := FCurveSec[(ord(whichStat) * 50) + index];
end;

procedure THeroRecord.reviseConditions(size: integer);
var i, dummy: word;
begin
   if high(FConditionModifiers) < size then
   begin
      dummy := high(FConditionModifiers);
      setLength(FConditionModifiers, size + 1);
      for I := dummy + 1 to high(FConditionModifiers) do
         FConditionModifiers[i] := 2;
      //end FOR
   end;
end;

function THeroRecord.StatSlice(low, high: integer): TIntArray;
var
   i: integer;
begin
   assert(high >= low);
   SetLength(result, (high - low) + 1);
   for i := Low to High do
      result[i - low] := FCurveSec[i];
end;

{ TRm2CharClass }

procedure TRm2CharClass.assign(const data: TRm2CharClass);
begin
assert(false);
end;

constructor TRm2CharClass.create(input: TStream; const id: byte; parent: TObject);
var
   converter: intX80;
   I: Integer;
begin
   assert(parent is TLcfDataBase);
   inherited create;
   FID := id;                   
   converter := TBerConverter.Create(input);
   if converter.getData <> id then
      raise EParseMessage.create('Hero record' + intToStr(id) + ' of RPG_RT.LDB not found!');
   FName := getStrSec(1, input, fillInBlankStr);
   FGraphicIndex := getNumSec(4, input, fillInHeroInt);
   FDualWield := getChboxSec($15, input, fillInHeroInt);
   FStaticEq := getChboxSec($16, input, fillInHeroInt);
   FComputerControlled := getChboxSec($17, input, fillInHeroInt);
   FStrongDefense := getChboxSec($18, input, fillInHeroInt);
   SetLength(FCurveSec, 1188);
   assert(getArraySec($1f, input, FCurveSec[0]) = length(FCurveSec));
   FExpStandard := getNumSec($29, input, fillInHeroInt);
   FExpAddition := getNumSec($2a, input, fillInHeroInt);
   FExpCorrection := getNumSec($2b, input, fillInHeroInt);
//Skill section
   FGraphicIndex := getNumSec($3e, input, fillInZeroInt);
   if peekAhead(input, $3f) then
   begin
      converter.read(input);
      converter.read(input);
      setLength(FSkillSection, converter.getData + 1);
      for I := 1 to high(FSkillSection) do
         FSkillSection[i] := THeroSkillRecord.create(input, i);
      //end FOR
   end
   else
      setLength(FSkillSection, 1);
   FSkillSection[0] := THeroSkillRecord.create(nil, 0);

   setLength(FConditionModifiers, getNumSec($47, input, fillInHeroInt) + 1);
   if length(FConditionModifiers) > 1 then
      getArraySec($48, input, FConditionModifiers[1])
   else
      getArraySec($48, input, FConditionModifiers[0]);
   setLength(FDtypeModifiers, getNumSec($49, input, fillInHeroInt) + 1);
   if length(FDtypeModifiers) > 1 then
      getArraySec($4a, input, FDtypeModifiers[1])
   else
      getArraySec($4a, input, FDtypeModifiers[0]);
   if GProjectFormat = pf_2k3 then
      assert(getArraySec($50, input, FBattleCommands) = 28);
   assert(peekAhead(input, 0));
end;

destructor TRm2CharClass.Destroy;
var i: integer;
begin
   for i := low(FSkillSection) to high(FSkillSection) do
      FSkillSection[i].Free;
   inherited;
end;

function TRm2CharClass.getCModifierCount: word;
begin
   result := high(FConditionModifiers);
end;

function TRm2CharClass.getCommand(x: integer): integer;
begin
   result := FBattleCommands[x];
end;

function TRm2CharClass.getConditionModifier(x: word): byte;
begin
   result := FConditionModifiers[x];
end;

function TRm2CharClass.getSkill(x: word): THeroSkillRecord;
begin
   if (x > 0) and (x <= high(FSkillSection)) then
      result := FSkillSection[x]
   else result := FSkillSection[0];
end;

function TRm2CharClass.getSkillCount: word;
begin
   result := high(FSkillSection);
end;

function TRm2CharClass.getStatCurve(whichStat: TGameStat; index: byte): word;
begin
   result := FCurveSec[(ord(whichStat) * 50) + index];
end;

procedure TRm2CharClass.reviseConditions(size: integer);
var i, dummy: word;
begin
   if high(FConditionModifiers) < size then
   begin
      dummy := high(FConditionModifiers);
      setLength(FConditionModifiers, size + 1);
      for I := dummy + 1 to high(FConditionModifiers) do
         FConditionModifiers[i] := 2;
      //end FOR
   end;
end;

function TRm2CharClass.getCurveSec: pointer;
begin
   result := @FCurveSec[0];
end;

function TRm2CharClass.getDModifierCount: word;
begin
   result := high(FDtypeModifiers);
end;

function TRm2CharClass.getDtypeModifier(x: word): byte;
begin
   result := FDtypeModifiers[x];
end;

{ THeroSkillRecord }

constructor THeroSkillRecord.create(input: TStream; const id: byte);
var
   converter: intX80;
begin
   inherited create;
   if input = nil then
      Exit;

   converter := TBerConverter.Create(input);
   if converter.getData <> id then
      raise EParseMessage.create('Hero skill record' + intToStr(id) + ' not found!');
   FLevel := getNumSec(1, input, fillInHskillInt);
   FSkill := getNumSec(2, input, fillInHSkillInt);
   assert(peekAhead(input, 0));
end;

{Classless}

const MAXEXP = 1000000;

function calcExp(currentLevel: integer; const stdIncrease, addIncrease, correction: integer; dummy: integer): integer;
var
   standard, additional: double;
   i: integer;
begin
   result := 0;
   standard := stdIncrease;
   additional := 1.5 + (addIncrease * 0.01);
   for i := currentLevel - 1 downto 1 do
   begin
      inc(result, correction + trunc(standard));
      standard := standard * additional;
      additional := (((currentLevel * 0.002) + 0.8) * (additional - 1) + 1);
   end; //end FOR
   result := min(result, MAXEXP);
end;

procedure fillInHeroInt(const expected: byte; out theResult: integer);
begin
   case expected of
      4, 5, $10, $15..$18, $2b, $42, $47, $49: theResult := 0;
      7, 9, $38: theResult := 1;
      8: theResult := 50;
      $0a, $29..$2a: theResult := 30;
   else
   begin
      msgBox ('No case implemented for x' + IntToHex(expected, 2) + '!', 'fillInHeroInt says:', MB_OK);
      raise EMessageAbort.Create
   end;
   end;
end;

procedure fillInHskillInt(const expected: byte; out theResult: integer);
begin
   case expected of
      1, 2: theResult := 1;
   else
      begin
         msgBox ('No case implemented for x' + IntToHex(expected, 2) + '!', 'fillInHskillInt says:', MB_OK);
         raise EMessageAbort.Create
      end;
   end;
end;

end.
