unit EB_System;

interface
uses
   EventBuilder, EB_Expressions, turbu_defs;

type
   TEBWait = class(TEBObject)
   public
      function GetNodeText: string; override;
      function GetScriptText: string; override;
   end;

   TEBGlobalSwitch = class(TEBObject)
   public
      constructor Create(parent: TEBObject; switch: TEBExpression; op: integer); reintroduce;
      function GetNodeText: string; override;
      function GetScriptText: string; override;
   end;

   TEBGlobalInt = class(TEBObject)
   public
      constructor Create(parent: TEBObject; int: TEBExpression; value: TEBExpression); reintroduce;
      function GetNodeText: string; override;
      function GetScriptText: string; override;
   end;

   TEBTimer = class(TEBObject)
   public
      function GetNodeText: string; override;
      function GetScriptText: string; override;
   end;

   TEBMoney = class(TEBObject)
   public
      function GetNodeText: string; override;
      function GetScriptText: string; override;
   end;

   TEBInventory = class(TEBObject)
   public
      function GetNodeText: string; override;
      function GetScriptText: string; override;
   end;

   TEBChangeParty = class(TEBObject)
   public
      function GetNodeText: string; override;
      function GetScriptText: string; override;
   end;

   TEBExperience = class(TEBObject)
   public
      function GetNodeText: string; override;
      function GetScriptText: string; override;
   end;

   TEBLevel = class(TEBObject)
   public
      function GetNodeText: string; override;
      function GetScriptText: string; override;
   end;

   TEBStats = class(TEBObject)
   public
      function GetNodeText: string; override;
      function GetScriptText: string; override;
   end;

   TEBSkills = class(TEBObject)
   public
      function GetNodeText: string; override;
      function GetScriptText: string; override;
   end;

   TEBEquipment = class(TEBObject)
   public
      function GetNodeText: string; override;
      function GetScriptText: string; override;
   end;

   TEBChangeHP = class(TEBObject)
   public
      function GetNodeText: string; override;
      function GetScriptText: string; override;
   end;

   TEBChangeMP = class(TEBObject)
   public
      function GetNodeText: string; override;
      function GetScriptText: string; override;
   end;

   TEBChangeStatus = class(TEBObject)
   public
      function GetNodeText: string; override;
      function GetScriptText: string; override;
   end;

   TEBFullHeal = class(TEBObject)
   public
      function GetNodeText: string; override;
      function GetScriptText: string; override;
   end;

   TEBTakeDamage = class(TEBObject)
   public
      function GetNodeText: string; override;
      function GetScriptText: string; override;
   end;

   TEBHeroName = class(TEBObject)
   public
      function GetNodeText: string; override;
      function GetScriptText: string; override;
   end;

   TEBHeroClass = class(TEBObject)
   public
      function GetNodeText: string; override;
      function GetScriptText: string; override;
   end;

   TEBHeroSprite = class(TEBObject)
   public
      function GetNodeText: string; override;
      function GetScriptText: string; override;
   end;

   TEBHeroPortrait = class(TEBObject)
   public
      function GetNodeText: string; override;
      function GetScriptText: string; override;
   end;

   TEBVehicleSprite = class(TEBObject)
   public
      function GetNodeText: string; override;
      function GetScriptText: string; override;
   end;

   TEBSysBGM = class(TEBObject)
   public
      function GetNodeText: string; override;
      function GetScriptText: string; override;
   end;

   TEBSysSFX = class(TEBObject)
   public
      function GetNodeText: string; override;
      function GetScriptText: string; override;
   end;

   TEBSysSkin = class(TEBObject)
   public
      function GetNodeText: string; override;
      function GetScriptText: string; override;
   end;

   TEBTranslucency = class(TEBObject)
   public
      function GetNodeText: string; override;
      function GetScriptText: string; override;
   end;

   TEBPlayBGM = class(TEBObject)
   public
      function GetNodeText: string; override;
      function GetScriptText: string; override;
   end;

   TEBFadeBGM = class(TEBObject)
   public
      function GetNodeText: string; override;
      function GetScriptText: string; override;
   end;

   TEBMemBGM = class(TEBObject)
   public
      function GetNodeText: string; override;
      function GetScriptText: string; override;
   end;

   TEBPlayMemBGM = class(TEBObject)
   public
      function GetNodeText: string; override;
      function GetScriptText: string; override;
   end;

   TEBPlaySFX = class(TEBObject)
   public
      function GetNodeText: string; override;
      function GetScriptText: string; override;
   end;

   TEBPlayMovie = class(TEBObject)
   public
      function GetNodeText: string; override;
      function GetScriptText: string; override;
   end;

   TEBInput = class(TEBObject)
   public
      function GetNodeText: string; override;
      function GetScriptText: string; override;
   end;

   TEBDeleteObj = class(TEBObject)
   public
      function GetNodeText: string; override;
      function GetScriptText: string; override;
   end;

   TEBCallEvent = class(TEBObject)
   private
      function EventName(id: integer): string;
   public
      function GetNodeText: string; override;
      function GetScriptText: string; override;
   end;

   TEBGameOver = class(TEBObject)
   public
      function GetNodeText: string; override;
      function GetScriptText: string; override;
   end;

   TEBTitleScreen = class(TEBObject)
   public
      function GetNodeText: string; override;
      function GetScriptText: string; override;
   end;

   TEBClassChange = class(TEBObject)
   private
      function CharClassName(id: integer): string;
   public
      function GetNodeText: string; override;
      function GetScriptText: string; override;
   end;

   TEBBattleCommand = class(TEBObject)
   public
      function GetNodeText: string; override;
      function GetScriptText: string; override;
   end;

implementation
uses
   SysUtils, Classes, TypInfo,
   EB_RpgScript;

function GetIntScript(decider, value: integer): string;
begin
   if boolean(decider) then
      result := intToStr(Value)
   else result := format('Ints[%d]', [value]);
end;

{ TEBWait }

function TEBWait.GetNodeText: string;
begin
   result := 'Wait ' + SecondFraction(values[0]);;
end;

function TEBWait.GetScriptText: string;
const
   LINE = 'Wait(%d);';
begin
   result := format(LINE, [Values[0]]);
end;

{ TEBGlobalSwitch }

constructor TEBGlobalSwitch.Create(parent: TEBObject; switch: TEBExpression; op: integer);
begin
   inherited Create(parent);
   Add(switch);
   Values.add(op);
end;

function TEBGlobalSwitch.GetNodeText: string;
const
   LINE = 'Switch[%s] %s';
   OPS: array [0..2] of string = ('OFF', 'ON', 'TOGGLE');
begin
   result := format(LINE, [(Components[0] as TEBExpression).GetNodeText, OPS[Values[0]]]);
end;

function TEBGlobalSwitch.GetScriptText: string;
const
   VAL = 'Switch[%s]';
var
   switch: TEBExpression;
   subval: string;
begin
   switch := self.components[0] as TEBExpression;
   subval := format(VAL, [switch.GetScript(0)]);
   result := subval + ' := ';
   case Values[0] of
      0: result := result + 'false;';
      1: result := result + 'true;';
      2: result := result + format('not %s;', [subval]);
      else raise ERPGScriptError.Create('Invalid switch op');
   end;
end;

{ TEBGlobalInt }

constructor TEBGlobalInt.Create(parent: TEBObject; int, value: TEBExpression);
begin
   inherited Create(parent);
   Add(int);
   Add(value);
end;

function TEBGlobalInt.GetNodeText: string;
const
   LINE = 'Set Ints[%s] to %s';
var
   int, value: TEBExpression;
begin
   int := self.components[0] as TEBExpression;
   value := self.components[1] as TEBExpression;
   result := format(LINE, [int.GetScript(0), value.GetScript(0)]);
end;

function TEBGlobalInt.GetScriptText: string;
const
   LINE = 'Ints[%s] := %s;';
var
   int, value: TEBExpression;
begin
   int := self.components[0] as TEBExpression;
   value := self.components[1] as TEBExpression;
   result := format(LINE, [int.GetScript(0), value.GetScript(0)]);
end;

{ TEBTimer }

function TEBTimer.GetNodeText: string;
begin
   if (values.count > 5) and (values[5] = 1) then
      result := 'Timer 2 '
   else result := 'Timer ';
   case Values[0] of
      0: if boolean(Values[1]) then
            result := format('%s Set: Ints[%d]', [result, Values[2]])
         else result := format('%s Set: %d:%.2d', [result, Values[2] div 60, Values[2] mod 60]);
      1: result := result + 'Start';
      2: result := result + 'Stop';
   end;
end;

function TEBTimer.GetScriptText: string;
begin
   if (values.count > 5) and (values[5] = 1) then
      result := 'timer2'
   else result := 'timer';
   case Values[0] of
      0: if boolean(Values[1]) then
            result := format('%s.time := Ints[%d];', [result, Values[2]])
         else result := format('%s.time := %d;', [result, Values[2]]);
      1: result := format('%s.start(%s, %s);', [result, BOOL_STR[Values[3]], BOOL_STR[Values[4]]]);
      2: result := result + '.pause;';
   end;
end;

{ TEBMoney }

function TEBMoney.GetNodeText: string;
const
   LINE = 'Change Money: %s $%s';
   SIGNS: array[0..1] of string = ('Add', 'Subtract');
begin
   result := format(LINE, [SIGNS[Values[0]], GetIntScript(values[1], values[2])]);
end;

function TEBMoney.GetScriptText: string;
const
   LINE = 'money := money %s %s;';
   SIGNS: array[0..1] of char = ('+', '-');
begin
   result := format(LINE, [SIGNS[Values[0]], GetIntScript(values[1], values[2])]);
end;

{ TEBInventory }

const
   ADDREM: array[0..1] of string = ('Add', 'Remove');

function TEBInventory.GetNodeText: string;
const LINE = 'Change Inventory: %s %s of %s';
begin
   result := format(LINE, [ADDREM[Values[0]], GetIntScript(Values[1], Values[2]),
                           (Components[0] as TEBExpression).GetNodeText]);
end;

function TEBInventory.GetScriptText: string;
const LINE = '%sItem(%s, %s);';
begin
   result := format(LINE, [ADDREM[Values[0]], GetIntScript(Values[1], Values[2]),
                           (Components[0] as TEBExpression).GetScript(0)]);
end;

{ TEBChangeParty }

function TEBChangeParty.GetNodeText: string;
const LINE = 'Change Party: %s %s';
begin
   result := format(LINE, [ADDREM[values[0]], (Components[0] as TEBExpression).GetNodeText]);
end;

function TEBChangeParty.GetScriptText: string;
var
   param: string;
begin
   param := (Components[0] as TEBExpression).GetScript(0);
   if boolean(Values[0]) then
      result := format('heroLeave(%s);', [param])
   else result := format('heroJoin(%s);', [param]);
end;

{ TEBExperience }

function TEBExperience.GetNodeText: string;
begin
   result := result + 'Change Experience: ';
   case Values[0] of
      0: result := result + '[All Members]';
      1: result := result + HeroName(Values[1]);
      2: result := result + format('Ints[%d]', [Values[1]]);
   end;
   if boolean(Values[2]) then
      result := result + 'Subtract '
   else result := result + 'Add ';
   result := result + GetIntScript(Values[3], Values[4]);
end;

function TEBExperience.GetScriptText: string;
const
   ADD_LINE = 'AddExp(%s, %s, %s)';
   REM_LINE = 'RemoveExp(%s, %s)';
var
   param1, param2: string;
begin
   case Values[0] of
      0: param1 := '-1';
      1: param1 := intToStr(Values[1]);
      2: param1 := format('Ints[%d]', [Values[1]]);
   end;
   param2 := GetIntScript(Values[3], Values[4]);
   if boolean(Values[2]) then
      result := format(REM_LINE, [param1, param2])
   else result := format(ADD_LINE, [param1, param2, BOOL_STR[Values[5]]]);
end;

{ TEBLevel }

function TEBLevel.GetNodeText: string;
begin
   result := result + 'Change Level: ';
   case Values[0] of
      0: result := result + '[All Members]';
      1: result := result + HeroName(Values[1]);
      2: result := result + format('Ints[%d]', [Values[1]]);
   end;
   if boolean(Values[2]) then
      result := result + 'Subtract '
   else result := result + 'Add ';
   result := result + GetIntScript(Values[3], Values[4]);
end;

function TEBLevel.GetScriptText: string;
const
   ADD_LINE = 'AddLevels(%s, %s, %s)';
   REM_LINE = 'RemoveLevels(%s, %s)';
var
   param1, param2: string;
begin
   case Values[0] of
      0: param1 := '-1';
      1: param1 := intToStr(Values[1]);
      2: param1 := format('Ints[%d]', [Values[1]]);
   end;
   param2 := GetIntScript(Values[3], Values[4]);
   if boolean(Values[2]) then
      result := format(REM_LINE, [param1, param2])
   else result := format(ADD_LINE, [param1, param2, BOOL_STR[Values[5]]]);
end;

{ TEBStats }

function TEBStats.GetNodeText: string;
const LINE = 'Change Stat: (%s): %s %s';
begin
   result := format(LINE, [(Components[0] as TEBExpression).GetNodeText,
                           ADDREM[Values[0]],
                           (Components[1] as TEBExpression).GetNodeText]);
end;

function TEBStats.GetScriptText: string;
const
   LINE = '%s := %s %s %s;';
   OPS: array[0..1] of char = ('+', '-');
var
   dummy: string;
begin
   dummy := (Components[0] as TEBExpression).GetScript(0);
   result := format(LINE, [dummy, dummy, OPS[Values[0]],
                           (Components[1] as TEBExpression).GetScript(0)]);
end;

{ TEBSkills }

function TEBSkills.GetNodeText: string;
const
   LINE = 'Change Skill: %s, %s %s';
   ACTION: array[0..1] of string = ('Learn', 'Forget');
begin
   result := format(LINE, [(Components[0] as TEBExpression).GetNodeText,
                           ACTION[values[0]],
                           (Components[1] as TEBExpression).GetNodeText]);
end;

function TEBSkills.GetScriptText: string;
const LINE = '%s.skill[%s] := %s;';
begin
   result := format(LINE, [(Components[0] as TEBExpression).GetScript(0),
                           (Components[1] as TEBExpression).GetScript(0),
                           BOOL_STR[values[0]]]);
end;

{ TEBEquipment }

function TEBEquipment.GetNodeText: string;
const
   LINE = 'Change Equipment: %s, %s %s';
   ACTION: array[0..1] of string = ('Equip', 'Unequip');
begin
   result := format(LINE, [(Components[0] as TEBExpression).GetNodeText,
                           ACTION[values[0]],
                           (Components[1] as TEBExpression).GetNodeText]);
end;

function TEBEquipment.GetScriptText: string;
const
   EQUIP_LINE = '%s.Equip(%s);';
   UNEQUIP_LINE = '%s.Unequip(%s);';
var
   line: string;
begin
   if boolean(Values[0]) then
      line := UNEQUIP_LINE
   else
      line := EQUIP_LINE;
   result := format(line, [(Components[0] as TEBExpression).GetScript(0),
                           (Components[1] as TEBExpression).GetScript(0)]);
end;

{ TEBChangeHP }

function TEBChangeHP.GetNodeText: string;
const
   LINE = 'Change HP: %s, %s %s';
   ACTION: array[0..1] of string = ('Gain', 'Lose');
begin
   result := format(LINE, [(Components[0] as TEBExpression).GetNodeText,
                           ACTION[values[0]],
                           (Components[1] as TEBExpression).GetNodeText]);
   if boolean(Values[1]) then
      result := result + ', Death Possible';
end;

function TEBChangeHP.GetScriptText: string;
const
   LINE = '%s.ChangeHP(%s%s, %s);';
   SIGN: array[0..1] of string = ('', '-');
begin
   result := format(LINE, [(Components[0] as TEBExpression).GetScript(0),
                           SIGN[values[0]],
                           (Components[1] as TEBExpression).GetScript(0),
                           BOOL_STR[values[1]]]);
end;

{ TEBChangeMP }

function TEBChangeMP.GetNodeText: string;
const
   LINE = 'Change MP: %s, %s %s';
   ACTION: array[0..1] of string = ('Gain', 'Lose');
begin
   result := format(LINE, [(Components[0] as TEBExpression).GetNodeText,
                           ACTION[values[0]],
                           (Components[1] as TEBExpression).GetNodeText]);
end;

function TEBChangeMP.GetScriptText: string;
const
   LINE = '%s.ChangeMP(%s%s);';
   SIGN: array[0..1] of string = ('', '-');
begin
   result := format(LINE, [(Components[0] as TEBExpression).GetScript(0),
                           SIGN[values[0]],
                           (Components[1] as TEBExpression).GetScript(0)]);
end;

{ TEBChangeStatus }

function TEBChangeStatus.GetNodeText: string;
const
   LINE = 'Change Status: %s, %s %s';
   ACTION: array[0..1] of string = ('Set', 'Clear');
begin
   result := format(LINE, [(Components[0] as TEBExpression).GetNodeText,
                           ACTION[values[0]],
                           (Components[1] as TEBExpression).GetNodeText]);
end;

function TEBChangeStatus.GetScriptText: string;
const LINE = '%s.condition[%s] := %s;';
begin
   result := format(LINE, [(Components[0] as TEBExpression).GetScript(0),
                           (Components[1] as TEBExpression).GetScript(0),
                           BOOL_STR[values[0]]]);
end;

{ TEBFullHeal }

function TEBFullHeal.GetNodeText: string;
const LINE = 'Full Heal: %s';
begin
   result := format(LINE, [(Components[0] as TEBExpression).GetNodeText]);
end;

function TEBFullHeal.GetScriptText: string;
const LINE = '%s.FullHeal;';
begin
   result := format(LINE, [(Components[0] as TEBExpression).GetScript(0)]);
end;

{ TEBTakeDamage }

function TEBTakeDamage.GetNodeText: string;
const LINE = 'Take Damage: %s, Power: %d, Def: %d, M.Def: %d, Variance: %d';
var
   subject: string;
begin
   case Values[0] of
      0: subject := 'Whole Party';
      1: subject := self.HeroName(Values[1]);
      2: subject := format('Hero[Ints[%d]]', [Values[1]]);
   end;
   result := format(LINE, [subject, values[2], values[3], values[4], values[5]]);
   if boolean(Values[6]) then
      result := format('%s, Store in Ints[%d]', [result, Values[7]]);
end;

function TEBTakeDamage.GetScriptText: string;
const LINE = '%s.TakeDamage(%d, %d, %d, %d);';
var
   subject: string;
begin
   case Values[0] of
      0: subject := 'party';
      1: subject := format('hero[%d]', [Values[1]]);
      2: subject := format('hero[Ints[%d]]', [Values[1]]);
   end;
   result := format(LINE, [subject, values[2], values[3], values[4], values[5]]);
   if boolean(Values[6]) then
      result := format('Ints[%d] := %s', [Values[7], result]);
end;

{ TEBHeroName }

function TEBHeroName.GetNodeText: string;
begin
   result := format('Rename Hero %s to %s', [HeroName(Values[0]), Text]);
end;

function TEBHeroName.GetScriptText: string;
begin
   result := format('hero[%d].name := %s;', [Values[0], QuotedStr(Text)]);
end;

{ TEBHeroClass }

function TEBHeroClass.GetNodeText: string;
begin
   result := format('Change Hero %s''s class to %s', [HeroName(Values[0]), Text]);
end;

function TEBHeroClass.GetScriptText: string;
begin
   result := format('hero[%d].CharClass := %s;', [Values[0], QuotedStr(Text)]);
end;

{ TEBHeroSprite }

function TEBHeroSprite.GetNodeText: string;
const LINE = 'Change Hero Sprite: %s, %s';
begin
   result := format(LINE, [HeroName(Values[0]), Text]);
   if boolean(Values[1]) then
      result := result + ' (Transparent)';
end;

function TEBHeroSprite.GetScriptText: string;
const LINE = 'hero[%d].SetSprite(%s, %s);';
begin
   result := format(LINE, [Values[0], QuotedStr(Text), BOOL_STR[Values[1]]]);
end;

{ TEBHeroPortrait }

function TEBHeroPortrait.GetNodeText: string;
const LINE = 'Set Portrait: %s, %s #%d';
begin
   result := format(LINE, [HeroName(Values[0]), Text, Values[1]]);
end;

function TEBHeroPortrait.GetScriptText: string;
const LINE = 'hero[%d].SetPortrait(%s, %d);';
begin
   result := format(LINE, [Values[0], QuotedStr(Text), Values[1]]);
end;

{ TEBVehicleSprite }

function TEBVehicleSprite.GetNodeText: string;
const
   LINE = 'Change Vehicle Sprite: %s, %s';
   VEHICLES: array[0..2] of string = ('Boat', 'Ship', 'Airship');
begin
   result := format(LINE, [VEHICLES[Values[0]], Text]);
end;

function TEBVehicleSprite.GetScriptText: string;
const LINE = 'vehicle[%d].SetSprite(%s);';
begin
   result := format(LINE, [Values[0], QuotedStr(Text)]);
end;

{ TEBSysBGM }

function TEBSysBGM.GetNodeText: string;
const LINE = 'Set System Music: %s, %s';
begin
   result := format(LINE, [CleanEnum(GetEnumName(TypeInfo(TBgmTypes), Values[0])), Text]);
end;

function TEBSysBGM.GetScriptText: string;
const
   LINE = 'SetSystemMusic(%s, %s, %d, %d, %d, %d);';
begin
   result := format(LINE, [GetEnumName(TypeInfo(TBgmTypes), Values[0]), QuotedStr(Text),
                           Values[1], Values[2], Values[3], Values[4]]);
end;

{ TEBSysSFX }

function TEBSysSFX.GetNodeText: string;
const LINE = 'Set System SFX: %s, %s';
begin
   result := format(LINE, [CleanEnum(GetEnumName(TypeInfo(TSfxTypes), Values[0])), Text]);
end;

function TEBSysSFX.GetScriptText: string;
const LINE = 'SetSystemSound(%s, %s, %d, %d, %d);';
begin
   result := format(LINE, [GetEnumName(TypeInfo(TSfxTypes), Values[0]), QuotedStr(Text),
                           Values[1], Values[2], Values[3]]);
end;

{ TEBSysSkin }

function TEBSysSkin.GetNodeText: string;
begin
   result := format('Change System Skin: %s', [Text]);
end;

function TEBSysSkin.GetScriptText: string;
begin
   result := format('SetSkin(%s);', [QuotedStr(Text)]);
end;

{ TEBTranslucency }

function TEBTranslucency.GetNodeText: string;
begin
   result := 'Set Party Translucency: ' + IntToStr(Values[0]);
end;

function TEBTranslucency.GetScriptText: string;
begin
   result := format('party.translucency := %d;', [Values[0]]);
end;

{ TEBPlayBGM }

function TEBPlayBGM.GetNodeText: string;
begin
   result := 'Play BGM: ' + Text;
end;

function TEBPlayBGM.GetScriptText: string;
const LINE = 'PlayMusic(%s, %d, %d, %d, %d);';
begin
   result := format(LINE, [QuotedStr(Text), Values[0], Values[1], Values[2], Values[3]]);
end;

{ TEBFadeBGM }

function TEBFadeBGM.GetNodeText: string;
begin
   result := 'Fade Out BGM: ' + SecondFraction(Values[0]);
end;

function TEBFadeBGM.GetScriptText: string;
begin
   result := format('FadeOutMusic(%d);', [Values[0]]);
end;

{ TEBMemBGM }

function TEBMemBGM.GetNodeText: string;
begin
   result := 'Memorize BGM';
end;

function TEBMemBGM.GetScriptText: string;
begin
   result := 'MemorizeBGM;';
end;

{ TEBPlayMemBGM }

function TEBPlayMemBGM.GetNodeText: string;
begin
   result := 'Play Memorized BGM';
end;

function TEBPlayMemBGM.GetScriptText: string;
begin
   result := 'PlayMemorizedBGM;';
end;

{ TEBPlaySFX }

function TEBPlaySFX.GetNodeText: string;
begin
   result := 'Play SFX: ' + Text;
end;

function TEBPlaySFX.GetScriptText: string;
const LINE = 'PlaySound(%s, %d, %d, %d)';
begin
   result := format(LINE, [QuotedStr(Text), Values[0], Values[1], Values[2]]);
end;

{ TEBPlayMovie }

function TEBPlayMovie.GetNodeText: string;
const LINE = 'Play Movie: %s, Position: (%.3d, %.3d), Size: (%dx%d)';
begin
   if boolean(Values[0]) then
      result := StringReplace(LINE, '%.3d', 'Ints[%d]', [rfReplaceAll])
   else result := LINE;
   result := format(result, [Text, Values[1], Values[2], Values[3], Values[4]]);
end;

function TEBPlayMovie.GetScriptText: string;
const LINE = 'PlayMovie(%s,  %d,  %d, %d, %d);';
begin
   if boolean(Values[0]) then //only replace the first two
      result := StringReplace(LINE, '  %d', ' Ints[%d]', [rfReplaceAll])
   else result := LINE;
   result := format(result, [Text, Values[1], Values[2], Values[3], Values[4]]);
end;

{ TEBInput }

function TEBInput.GetNodeText: string;
begin
   result := format('Key Input: Ints[%d]', [Values[0]]);
   if boolean(Values[1]) then
      result := result + ' (Wait)';
end;

function TEBInput.GetScriptText: string;
const LINE = 'ints[%d] := keyScan($s, %s);';
var
   mask: string;

   procedure AddMask(const value: string);
   begin
      if mask <> '' then
         mask := mask + ' + ';
      mask := mask + value;
   end;

begin
   mask := '';
   //key codes
   if boolean(Values[2]) and boolean(Values[3]) and boolean(Values[4]) then
      mask := 'KS_ALL'
   else begin
      if boolean(Values[2]) then
         AddMask('KS_DIRS');
      if boolean(Values[3]) then
         AddMask('KS_ACTION');
      if boolean(Values[3]) then
         AddMask('KS_CANCEL');
      if mask = '' then
         mask := '[]';
   end;
   result := format(LINE, [Values[0], mask, BOOL_STR[Values[1]]]);
end;

{ TEBDeleteObj }

function TEBDeleteObj.GetNodeText: string;
begin
   result := 'Delete Map Object';
   if boolean(Values[0]) then
      result := result + '(Permanant)';
end;

function TEBDeleteObj.GetScriptText: string;
begin
   result := format('DeleteObject(%s)', [BOOL_STR[Values[0]]]);
end;

{ TEBCallEvent }

function TEBCallEvent.EventName(id: integer): string;
begin
   result := GetLookup(id, 'MapObjects');
end;

function TEBCallEvent.GetNodeText: string;
begin
   result := 'Call Event: ';
   case Values[0] of
      0: result := format('Common Event #%d', [Values[1]]);
      1:
      begin
         if Values[1] <> 10005 then
            result := format('%s, Page %d', [EventName(Values[1]), Values[2]])
         else
            result := format('This Event, Page %d', [Values[2]]);
      end;
      2: result := format('#Ints[%d], Page Ints[%d]', [Values[1], Values[2]]);
   end;
end;

function TEBCallEvent.GetScriptText: string;
begin
   case Values[0] of
      0: result := format('callGlobalEvent(%d);', [Values[1]]);
      1:
      begin
         if Values[1] <> 10005 then
            result := format('callEvent(%d, %d);', [Values[1], Values[2]])
         else
            result := format('callEvent(thisEvent.id, %d);', [Values[2]]);
      end;
      2: result := format('callEvent(Ints[%d], Ints[%d]);', [Values[1], Values[2]]);
   end;
end;

{ TEBGameOver }

function TEBGameOver.GetNodeText: string;
begin
   result := 'Game Over';
end;

function TEBGameOver.GetScriptText: string;
begin
   result := 'GameOver;';
end;

{ TEBTitleScreen }

function TEBTitleScreen.GetNodeText: string;
begin
   result := 'Go To Title Screen';
end;

function TEBTitleScreen.GetScriptText: string;
begin
   result := 'TitleScreen;';
end;

{ TEBClassChange }

function TEBClassChange.CharClassName(id: integer): string;
begin
   result := GetLookup(id, 'CharClasses');
end;

function TEBClassChange.GetNodeText: string;
begin
   result := format('Change Class: %s, %s', [HeroName(Values[0]), CharClassName(Values[1])]);
end;

function TEBClassChange.GetScriptText: string;
const LINE = 'ChangeClass(%d, %d, %s, %d, %d, %s);';
begin
   result := format(LINE, [Values[0], Values[1], BOOL_STR[Values[2]], Values[3],
                           Values[4], BOOL_STR[Values[5]]]);
end;

{ TEBBattleCommand }

function TEBBattleCommand.GetNodeText: string;
const LINE = 'Change Battle Commands: %s %s %s';
var
   name: string;
begin
   if Values[1] = 0 then
      name := '(All)'
   else name := GetLookup(Values[1], 'commands');
   result := format(LINE, [HeroName(Values[0]), ADDREM[Values[2]], name]);
end;

function TEBBattleCommand.GetScriptText: string;
const
   LINE = 'hero[%d].%sBattleCommand(%d)';
begin
   result := format(LINE, [Values[0], ADDREM[Values[2], Values[1]]]);
end;

initialization
   RegisterClasses([TEBWait, TEBGlobalSwitch, TEBGlobalInt, TEBTimer, TEBMoney,
                    TEBInventory, TEBChangeParty, TEBExperience, TEBLevel, TEBStats,
                    TEBSkills, TEBEquipment, TEBChangeHP, TEBChangeStatus, TEBFullHeal,
                    TEBTakeDamage, TEBHeroName, TEBHeroClass, TEBHeroSprite,
                    TEBHeroPortrait, TEBVehicleSprite, TEBSysBGM, TEBSysSFX,
                    TEBSysSkin, TEBTranslucency, TEBPlayBGM, TEBFadeBGM, TEBMemBGM,
                    TEBPlayMemBGM, TEBPlaySFX, TEBPlayMovie, TEBInput, TEBDeleteObj,
                    TEBCallEvent, TEBGameOver, TEBTitleScreen, TEBClassChange,
                    TEBBattleCommand]);
end.
