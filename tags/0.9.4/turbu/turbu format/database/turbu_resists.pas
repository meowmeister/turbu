unit turbu_resists;
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
   classes, DB,
   turbu_defs, turbu_classes;

type
   TAttackLimitation = (al_none, al_paralyze, al_berserk, al_charm);
   TConditionMessages = (cm_ally, cm_enemy, cm_already, cm_normal, cm_end);
   TConditionMessageSet = array[TConditionMessages] of string;
   TStatEffect = (se_half, se_double, se_none);
   TDotEffect = (de_none, de_regen, de_damage);
   TResistArray = array[0..4] of smallint;

   TRpgResistable = class(TRpgDatafile)
   private
      FStandard: TResistArray;
      function GetStandard(x: integer): smallint;
      procedure SetStandard(x: integer; const Value: smallint);
   public
      constructor Load(savefile: TStream); override;
      procedure save(savefile: TStream); override;

      property standard[x: integer]: smallint read GetStandard write SetStandard;
   end;

   TConditionTemplate = class(TRpgResistable)
   protected
      FOutOfBattle: boolean;
      FColor: integer;
      FPriority: integer;
      FAttackLimit: TAttackLimitation;
      FHealTurns: integer;
      FHealPercent: integer;
      FHealShock: integer;
      FAttackStat: boolean;
      FDefenseStat: boolean;
      FMindStat: boolean;
      FSpeedStat: boolean;
      FToHitChange: integer;
      FPhysBlock: boolean;
      FPhysCutoff: integer;
      FMagBlock: boolean;
      FMagCutoff: integer;
      FConditionMessages: TConditionMessageSet;
      FUsesConditionMessages: boolean;
      FHpTurnPercent: integer;
      FHpTurnFixed: integer;
      FHpStepCount: integer;
      FHpStepQuantity: integer;
      FMpTurnPercent: integer;
      FMpTurnFixed: integer;
      FMpStepCount: integer;
      FMpStepQuantity: integer;

      //2003 fields
      FStatEffect: TStatEffect;
      FEvade: boolean;
      FReflect: boolean;
      FEqLock: boolean;
      FStatusAnimation: integer;
      FHpDot: TDotEffect;
      FMpDot: TDotEffect;
      FTag: T4IntArray;
   protected
      class function keyChar: ansiChar; override;
   public
      constructor Load(savefile: TStream); override;
      procedure save(savefile: TStream); override;

      property outOfBattle: boolean read FOutOfBattle write FOutOfBattle;
      property color: integer read FColor write FColor;
      property priority: integer read FPriority write FPriority;
      property attackLimit: TAttackLimitation read FAttackLimit write FAttackLimit;

      property healTurns: integer read FHealTurns write FHealTurns;
      property healTurnPercent: integer read FHealPercent write FHealPercent;
      property healShockPercent: integer read FHealShock write FHealShock;
      property attack: boolean read FAttackStat write FAttackStat;
      property defense: boolean read FDefenseStat write FDefenseStat;
      property mind: boolean read FMindStat write FMindStat;
      property speed: boolean read FSpeedStat write FSpeedStat;
      property toHitChange: integer read FToHitChange write FToHitChange;
      property physBlock: boolean read FPhysBlock write FPhysBlock;
      property magBlock: boolean read FMagBlock write FMagBlock;
      property physCutoff: integer read FPhysCutoff write FPhysCutoff;
      property magCutoff: integer read FMagCutoff write FMagCutoff;
      property conditionMessages: TConditionMessageSet read FConditionMessages write FConditionMessages;
      property usesConditionMessages: boolean read FUsesConditionMessages write FUsesConditionMessages;
      property hpTurnPercent: integer read FHpTurnPercent write FHpTurnPercent;
      property hpTurnFixed: integer read FHpTurnFixed write FHpTurnFixed;
      property hpStepCount: integer read FHpStepCount write FHpStepCount;
      property hpStepQuantity: integer read FHpStepQuantity write FHpStepQuantity;
      property mpTurnPercent: integer read FMpTurnPercent write FMpTurnPercent;
      property mpTurnFixed: integer read FMpTurnFixed write FMpTurnFixed;
      property mpStepCount: integer read FMpStepCount write FMpStepCount;
      property mpStepQuantity: integer read FMpStepQuantity write FMpStepQuantity;

      property statEffect: TStatEffect read FStatEffect write FStatEffect;
      property evade: boolean read FEvade write FEvade;
      property reflect: boolean read FReflect write FReflect;
      property eqLock: boolean read FEqLock write FEqLock;
      property animation: integer read FStatusAnimation write FStatusAnimation;
      property hpDot: TDotEffect read FHpDot write FHpDot;
      property mpDot: TDotEffect read FMpDot write FMpDot;
      property tag: T4IntArray read FTag write FTag;
   end;

   TAttributeTemplate = class(TRpgResistable)
   private
      FRequiredForSkills: boolean;
   protected
      class function keyChar: ansiChar; override;
   public
      constructor Load(savefile: TStream); override;
      procedure save(savefile: TStream); override;

      property requiredForSkills: boolean read FRequiredForSkills write FRequiredForSkills;
   end;

implementation

{ TConditionTemplate }

class function TConditionTemplate.keyChar: ansiChar;
begin
   result := 'c';
end;

constructor TConditionTemplate.Load(savefile: TStream);
begin
   inherited Load(savefile);
   FOutOfBattle := savefile.readBool;
   FColor := savefile.readByte;
   FPriority := savefile.readByte;
   savefile.readBuffer(FAttackLimit, sizeof(TAttackLimitation));
   savefile.readBuffer(FTag, sizeof(T4IntArray));
   lassert(savefile.readChar = 'C');
end;

procedure TConditionTemplate.save(savefile: TStream);
begin
   inherited save(savefile);
   savefile.writeBool(FOutOfBattle);
   savefile.writeByte(FColor);
   savefile.writeByte(FPriority);
   savefile.WriteBuffer(FAttackLimit, sizeof(TAttackLimitation));
   savefile.WriteBuffer(FTag, sizeof(T4IntArray));
   savefile.writeChar('C');
end;

{ TAttributeTemplate }

class function TAttributeTemplate.keyChar: ansiChar;
begin
   result := 'a';
end;

constructor TAttributeTemplate.Load(savefile: TStream);
begin
   inherited Load(savefile);
   FRequiredForSkills := savefile.readBool;
   lassert(savefile.readChar = 'A');
end;

procedure TAttributeTemplate.save(savefile: TStream);
begin
   inherited save(savefile);
   savefile.writeBool(FRequiredForSkills);
   savefile.writeChar('A');
end;

{ TRpgResistable }

constructor TRpgResistable.Load(savefile: TStream);
begin
   inherited Load(savefile);
   savefile.Read(FStandard, sizeof(FStandard));
end;

procedure TRpgResistable.save(savefile: TStream);
begin
   inherited Save(savefile);
   savefile.write(FStandard, sizeof(FStandard));
end;

procedure TRpgResistable.SetStandard(x: integer; const Value: smallint);
begin
   if x in [0..4] then
      FStandard[x] := value;
end;

function TRpgResistable.GetStandard(x: integer): smallint;
begin
   if x in [0..4] then
      result := FStandard[x]
   else result := 0;
end;

end.
