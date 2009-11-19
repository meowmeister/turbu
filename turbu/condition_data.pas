unit condition_data;
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
   classes;

type
   TAttackLimitation = (al_none, al_paralyze, al_berserk, al_charm);
   TStatEffect = (se_half, se_double, se_none);
   TDotEffect = (de_none, de_regen, de_damage);
   TConditionMessages = (cm_ally, cm_enemy, cm_already, cm_normal, cm_end);

   TConditionMessageSet = array[TConditionMessages] of ansiString;

   TCondition = class(TObject)
   private
      FName: ansiString;
      FEffectChance: array[1..5] of byte;
      FColor: byte;
      FPriority: byte;
      FLimitation: TAttackLimitation;
      FHealTurns: word;
      FHealPercent: byte;
      FHealShock: byte;
      FAttackStat: boolean;
      FDefenseStat: boolean;
      FMindStat: boolean;
      FSpeedStat: boolean;
      FToHitChange: byte;
      FPhysBlock: boolean;
      FPhysCutoff: word;
      FMagBlock: boolean;
      FMagCutoff: word;
      FConditionMesages: TConditionMessageSet;
      FUsesConditionMessages: boolean;
      FHpTurnPercent: byte;
      FHpTurnFixed: word;
      FHpStepCount: byte;
      FHpStepQuantity: word;
      FMpTurnPercent: byte;
      FMpTurnFixed: word;
      FMpStepCount: byte;
      FMpStepQuantity: word;

      //2003 fields
      FStatEffect: TStatEffect;
      FEvade: boolean;
      FReflect: boolean;
      FEqLock: boolean;
      FStatusAnimation: byte;
      FHpDot: TDotEffect;
      FMpDot: TDotEffect;
      function getChance(x: byte): byte;
   public
      constructor Create(input: TStream; const id: word);

      property name: ansiString read FName;
      property chance[x: byte]: byte read getChance;
      property color: byte read FColor;
      property priority: byte read FPriority;
      property limitation: TAttackLimitation read FLimitation;
      property healTurns: word read FHealTurns;
      property healTurnPercent: byte read FHealPercent;
      property healShockPercent: byte read FHealShock;
      property attack: boolean read FAttackStat;
      property defense: boolean read FDefenseStat;
      property mind: boolean read FMindStat;
      property speed: boolean read FSpeedStat;
      property toHitChange: byte read FToHitChange;
      property physBlock: boolean read FPhysBlock;
      property magBlock: boolean read FMagBlock;
      property physCutoff: word read FPhysCutoff;
      property magCutoff: word read FMagCutoff;
      property conditionMessages: TConditionMessageSet read FConditionMesages;
      property usesConditionMessages: boolean read FUsesConditionMessages;
      property hpTurnPercent: byte read FHpTurnPercent;
      property hpTurnFixed: word read FHpTurnFixed;
      property hpStepCount: byte read FHpStepCount;
      property hpStepQuantity: word read FHpStepQuantity;
      property mpTurnPercent: byte read FMpTurnPercent;
      property mpTurnFixed: word read FMpTurnFixed;
      property mpStepCount: byte read FMpStepCount;
      property mpStepQuantity: word read FMpStepQuantity;

      property stat_effect: TStatEffect read FStatEffect;
      property evade: boolean read FEvade;
      property reflect: boolean read FReflect;
      property eqLock: boolean read FEqLock;
      property animation: byte read FStatusAnimation;
      property hpDot: TDotEffect read FHpDot write FHpDot;
      property mpDot: TDotEffect read FMpDot write FMpDot;
   end;

implementation
uses sysUtils, windows,
     commons, formats, fileIO, BER;

procedure fillInCondInt(const expected: byte; out theResult: integer); forward;

{ TCondition }

constructor TCondition.Create(input: TStream; const id: word);
var
   converter: intX80;
   i: Integer;
begin
   inherited create;
   converter := TBerConverter.Create(input);
   if converter.getData <> id then
      raise EParseMessage.create('Condition record' + intToStr(id) + ' of RPG_RT.LDB not found!');
   FName := getStrSec(1, input, fillInBlankStr);
   skipSec(2, input);
   FColor := getNumSec(3, input, fillInCondInt);
   FPriority := getNumSec(4, input, fillInCondInt);
   FLimitation := TAttackLimitation(getNumSec(5, input, fillInZeroInt));
   for i := 1 to 5 do
      FEffectChance[i] := getNumSec($A + i, input, fillInCondInt);
   FHealTurns := getNumSec($15, input, fillInZeroInt);
   FHealPercent := getNumSec($16, input, fillInZeroInt);
   FHealShock := getNumSec($17, input, fillInZeroInt);
   if GProjectFormat = pf_2k3 then
      FStatEffect := TStatEffect(getNumSec($1E, input, fillInZeroInt));
   FAttackStat := getChboxSec($1F, input, fillInZeroInt);
   FDefenseStat := getChboxSec($20, input, fillInZeroInt);
   FMindStat := getChboxSec($21, input, fillInZeroInt);
   FSpeedStat := getChboxSec($22, input, fillInZeroInt);
   FToHitChange := getNumSec($23, input, fillInZeroInt);
   if GProjectFormat = pf_2k3 then
   begin
      FEvade := getChboxSec($24, input, fillInZeroInt);
      FReflect := getChboxSec($25, input, fillInZeroInt);
      FEqLock := getChboxSec($26, input, fillInZeroInt);
      FStatusAnimation := getNumSec($27, input, fillInCondInt);
   end;
   FPhysBlock := getChboxSec($29, input, fillInZeroInt);
   FPhysCutoff := getNumSec($2A, input, fillInZeroInt);
   FMagBlock := getChboxSec($2B, input, fillInZeroInt);
   FMagCutoff := getNumSec($2C, input, fillInZeroInt);
   if GProjectFormat = pf_2k3 then
   begin
      FHpDot := TDotEffect(getNumSec($2D, input, fillInZeroInt));
      FMpDot := TDotEffect(getNumSec($2E, input, fillInZeroInt));
   end
   else begin
      FHpDot := de_damage;
      FMpDot := de_damage;
   end;
   FUsesConditionMessages := GProjectFormat = pf_2k;
   for I := 0 to 4 do
      FConditionMesages[TConditionMessages(i)] := getStrSec($33 + i, input, fillInBlankStr);
   FHpTurnPercent := getNumSec($3D, input, fillInZeroInt);
   FHpTurnFixed := getNumSec($3E, input, fillInZeroInt);
   FHpStepCount := getNumSec($3F, input, fillInZeroInt);
   FHpStepQuantity := getNumSec($40, input, fillInZeroInt);
   FMpTurnPercent := getNumSec($41, input, fillInZeroInt);
   FMpTurnFixed := getNumSec($42, input, fillInZeroInt);
   FMpStepCount := getNumSec($43, input, fillInZeroInt);
   FMpStepQuantity := getNumSec($44, input, fillInZeroInt);
   if not peekAhead(input, 0) then
      raise EParseMessage.create('Exceptional case found at LDB condition x' + intToHex(id, 2) + '!');
end;

function TCondition.getChance(x: byte): byte;
begin
   if not (x in [1..5]) then
      result := 0
   else
      result := FEffectChance[x];
end;

{ Classless }

procedure fillInCondInt(const expected: byte; out theResult: integer);
begin
   case expected of
      3: theResult := 6;
      4: theResult := 50;
      $0b: theResult := 100;
      $0c: theResult := 80;
      $0d: theResult := 60;
      $0e: theResult := 30;
      $0f: theResult := 0;
      $27: theResult := 6;
   else
   begin
      msgBox ('No case implemented for x' + IntToHex(expected, 2) + '!', 'fillInCondStr says:', MB_OK);
      raise EMessageAbort.Create
   end;
   end;
end;

end.
