unit skill_data;
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
   classes,
   rm_sound;

type
   TSkillType = (sk_normal, sk_teleport, sk_escape, sk_switch);
   TSkillRange = (sr_attack1, sr_attackAll, sr_self, sr_help1, sr_helpAll);

   TSkill = class(TObject)
   private
      FId: word;
      FName: ansiString;
      FDesc: ansiString;
      FUsage: ansiString;
      FUsage2: ansiString;
      FFailure: byte;
      FType: TSkillType;
      FCost: word;
      FRange: TSkillRange;
      FSwitch: word;
      FAnim: word;
      FSfx: TRmSound;
      FField: boolean;
      FBattle: boolean;
      FStrengthBase: byte;
      FMindBase: byte;
      FVariance: byte;
      FBase: word;
      FSuccessRate: byte;
      FHp: boolean;
      FMp: boolean;
      FAttack: boolean;
      FDefense: boolean;
      FMind: boolean;
      FSpeed: boolean;
      FVampire: boolean;
      FPhased: boolean;
      FCondition: array of boolean;
      FAttribute: array of boolean;
      FResistMod: boolean;

      //2003
      FUsesPercentCost: boolean;
      FPercentCost: byte;
      FInflictReversed: boolean; //refers to radio box under conditions
      FUnk32: ansiString;

      function canUseOnField: boolean;
      function getAttribute(x: word): boolean;
      function getCondition(x: word): boolean;
   public
      constructor Create(input: TStream; const id: word; parent: TObject);
      destructor Destroy; override;
      procedure reviseConditions(size: integer);

      property id: word read FId;
      property name: ansiString read FName;
      property desc: ansiString read FDesc;
      property usage: ansiString read FUsage;
      property usage2: ansiString read FUsage2;
      property failure: byte read FFailure;
      property skillType: TSkillType read FType;
      property cost: word read FCost;
      property range: TSkillRange read FRange;
      property switch: word read FSwitch;
      property anim: word read FAnim;
      property sfx: TrmSound read FSfx;
      property field: boolean read FField;
      property battle: boolean read FBattle;
      property strEffect: byte read FStrengthBase;
      property mindEffect: byte read FMindBase;
      property variance: byte read FVariance;
      property base: word read FBase;
      property successRate: byte read FSuccessRate;
      property hp: boolean read FHp;
      property mp: boolean read FMp;
      property attack: boolean read FAttack;
      property defense: boolean read FDefense;
      property mind: boolean read FMind;
      property speed: boolean read FSpeed;
      property drain: boolean read FVampire;
      property phased: boolean read FPhased;
      property condition[x: word]: boolean read getCondition;
      property attribute[x: word]: boolean read getAttribute;
      property usableOnField: boolean read canUseOnField;
      property usesPercentCost: boolean read FUsesPercentCost;
      property percentCost: byte read FPercentCost;
      property resistMod: boolean read FResistMod;
   end;

implementation
uses windows, sysUtils,
     commons, formats, fileIO, BER, LDB;

procedure fillInSkillStr(const expected: byte; out theResult: string); forward;
procedure fillInSkillInt(const expected: byte; out theResult: integer); forward;

{ TSkill }

function TSkill.canUseOnField: boolean;
begin
   result := (range in [sr_self, sr_help1, sr_helpAll]) and (self.hp or self.mp);
end;

constructor TSkill.Create(input: TStream; const id: word; parent: TObject);
var
   converter: intX80;
   i, dummy: word;
begin
   inherited create;
   converter := TBerConverter.Create(input);
   if converter.getData <> id then
      raise EParseMessage.create('Skill record' + intToStr(id) + ' of RPG_RT.LDB not found!');
   FId := id;
   FName := getStrSec(1, input, fillInBlankStr);
   FDesc := getStrSec(2, input, fillInBlankStr);
   FUsage := getStrSec(3, input, fillInBlankStr);
   FUsage2 := getStrSec(4, input, fillInBlankStr);
   FFailure := getNumSec(7, input, fillInZeroInt);
   FType := TSkillType(getNumSec(8, input, fillInZeroInt));
   if GProjectFormat = pf_2k3 then
   begin
      FUsesPercentCost := getChboxSec(9, input, fillInZeroInt);
      FPercentCost := getNumSec($A, input, fillInZeroInt);
   end;
   FCost := getNumSec($B, input, fillInZeroint);
   FRange := TSkillRange(getNumSec($C, input, fillInZeroInt));
   FSwitch := getNumSec($D, input, fillInZeroInt);
   FAnim := getNumSec($E, input, fillInZeroInt);
   FSfx := TRmSound.Create($10, input);
   FField := getChboxSec($12, input, fillInZeroInt);
   FBattle := getChboxSec($13, input, fillInZeroInt);
   if GProjectFormat = pf_2k3 then
      FInflictReversed := getChboxSec($14, input, fillInZeroInt);
   FStrengthBase := getNumSec($15, input, fillInZeroInt);
   FMindBase := getNumSec($16, input, fillInSkillInt);
   FVariance := getNumSec($17, input, fillInSkillInt);
   FBase := getNumSec($18, input, fillInZeroInt);
   FSuccessRate := getNumSec($19, input, fillInSkillInt);
   FHp := getChboxSec($1F, input, fillInZeroInt);
   FMp := getChboxSec($20, input, fillInZeroInt);
   FAttack := getChboxSec($21, input, fillInZeroInt);
   FDefense := getChboxSec($22, input, fillInZeroInt);
   FMind := getChboxSec($23, input, fillInZeroInt);
   FSpeed := getChboxSec($24, input, fillInZeroInt);
   FVampire := getChboxSec($25, input, fillInZeroInt);
   FPhased := getChboxSec($26, input, fillInZeroInt);

   setLength(FCondition, getNumSec($29, input, fillInZeroInt) + 1);
   if length(FCondition) > 1 then
      getArraySec($2A, input, FCondition[1])
   else
      getArraySec($2A, input, FCondition[0]);

   setLength(FAttribute, getNumSec($2B, input, fillInZeroInt) + 1);
   if length(FAttribute) = 1 then
   begin
      setLength(FAttribute, TLcfDataBase(parent).attributes + 1);
      dummy := 0;
      skipSec($2C, input);
   end else dummy := getArraySec($2C, input, FAttribute[1]);
   if dummy = 0 then
      for I := 1 to high(FAttribute) do
         FAttribute[i] := false
      //end for
   //end if
   else assert(dummy = high(FAttribute));

   FResistMod := getChboxSec($2D, input, fillInZeroInt);
   if GProjectFormat = pf_2k3 then
   begin
      skipSec($31, input); //Purely visual element: which character to display in the editor's example
      FUnk32 := getStrSec($32, input, nil);
   end;
   if not peekAhead(input, 0) then
      raise EParseMessage.create('Exceptional case found at LDB skill x' + intToHex(id, 2) + '!');
end;

destructor TSkill.Destroy;
begin
   FSfx.free;
   inherited;
end;

procedure TSkill.reviseConditions(size: integer);
var i, dummy: word;
begin
   if high(FCondition) < size then
   begin
      dummy := high(FCondition);
      setLength(FCondition, size + 1);
      for I := dummy + 1 to high(FCondition) do
         FCondition[i] := false;
      //end FOR
   end;
end;

function TSkill.getAttribute(x: word): boolean;
begin
   if x > high(FAttribute) then
      result := false
   else result := FAttribute[x];
end;

function TSkill.getCondition(x: word): boolean;
begin
   if x > high(FCondition) then
      result := false
   else result := FCondition[x];
end;

{ Classless }
procedure fillInSkillStr(const expected: byte; out theResult: string);
begin
   case expected of
      1: theResult := '';
   else
   begin
      msgBox ('No case implemented for x' + IntToHex(expected, 2) + '!', 'fillInSkillStr says:', MB_OK);
      raise EMessageAbort.Create
   end;
   end;
end;

procedure fillInSkillInt(const expected: byte; out theResult: integer);
begin
   case expected of
      $16: theResult := 3;
      $17: theResult := 4;
      $19: theResult := 100;
   else
   begin
      msgBox ('No case implemented for x' + IntToHex(expected, 2) + '!', 'fillInSkillStr says:', MB_OK);
      raise EMessageAbort.Create
   end;
   end;
end;

end.
