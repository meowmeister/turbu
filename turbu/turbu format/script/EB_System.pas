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

unit EB_System;

interface
uses
   Classes,
   EventBuilder, EB_Expressions, turbu_defs;

type
   TEBWait = class(TEBObject)
   public
      function GetNodeText: string; override;
      function GetScriptText: string; override;
   end;

   TEBSwitch = class(TEbObject)
   protected
      function GetSwitchName: string; virtual; abstract;
   public
      function GetNodeText: string; override;
      function GetScriptText: string; override;
   end;

   TEBGlobalSwitch = class(TEBSwitch)
   protected
      function GetSwitchName: string; override;
      procedure NeededVariables(list: TStringList); override;
   public
      constructor Create(parent: TEBObject; switch: TEBExpression; op: integer); reintroduce;
      function NeededVariableType: THeaderItems; override;
   end;

   TEBLocalSwitch = class(TEBSwitch)
   protected
      function GetSwitchName: string; override;
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

implementation
uses
   SysUtils, TypInfo,
   EB_RpgScript;

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

{ TEBSwitch }

function TEBSwitch.GetNodeText: string;
const
   OPS: array [0..2] of string = ('OFF', 'ON', 'TOGGLE');
begin
   result := GetSwitchName + ' ' + OPS[Values[0]];
end;

function TEBSwitch.GetScriptText: string;
var
   subval: string;
begin
   subval := GetSwitchName;
   result := subval + ' := ';
   case Values[0] of
      0: result := result + 'false;';
      1: result := result + 'true;';
      2: result := result + format('not %s;', [subval]);
      else raise ERPGScriptError.Create('Invalid switch op');
   end;
end;

{ TEBGlobalSwitch }

constructor TEBGlobalSwitch.Create(parent: TEBObject; switch: TEBExpression; op: integer);
begin
   inherited Create(parent);
   Add(switch);
   Values.add(op);
end;

function TEBGlobalSwitch.GetSwitchName: string;
begin
   result := format('Switch[%s]', [self.ChildScript[0]]);
end;

procedure TEBGlobalSwitch.NeededVariables(list: TStringList);
var
   subscript: TEBVariableValue;
begin
   if (Children[0] is TEBVariableValue) then
   begin
      subscript := TEbVariableValue(Children[0]);
      if not subscript.Global then
         list.Values[subscript.Text] := 'integer';
   end;
   inherited NeededVariables(list);
end;

function TEBGlobalSwitch.NeededVariableType: THeaderItems;
begin
   result := hi_var;
end;

{ TEBLocalSwitch }

function TEBLocalSwitch.GetSwitchName: string;
begin
   result := self.Text;
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
   int := self.Children[0] as TEBExpression;
   value := self.Children[1] as TEBExpression;
   result := format(LINE, [int.GetNodeText, value.GetNodeText]);
end;

function TEBGlobalInt.GetScriptText: string;
const
   LINE = 'Ints[%s] := %s;';
var
   int, value: TEBExpression;
begin
   int := self.Children[0] as TEBExpression;
   value := self.Children[1] as TEBExpression;
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
            result := format('%s Set: Ints[%s]', [result, IntName(Values[2])])
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

{ TEBInput }

function TEBInput.GetNodeText: string;
begin
   result := format('Key Input: Ints[%s]', [IntName(Values[0])]);
   if boolean(Values[1]) then
      result := result + ' (Wait)';
end;

function TEBInput.GetScriptText: string;
const LINE = 'ints[%d] := keyScan(%s, %s);';
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
   result := format('DeleteObject(%s);', [BOOL_STR[Values[0]]]);
end;

{ TEBCallEvent }

function TEBCallEvent.EventName(id: integer): string;
begin
   result := GetLookup(id, 'MapObjects');
end;

function TEBCallEvent.GetNodeText: string;
begin
   case Values[0] of
      0: result := format('Global Script #%d', [Values[1]]);
      1:
      begin
         if Values[1] <> 10005 then
            result := format('%s, Page %d', [EventName(Values[1]), Values[2]])
         else
            result := format('This Object, Page %d', [Values[2]]);
      end;
      2: result := format('Object # Ints[%s], Page Ints[%s]', [IntName(Values[1]), IntName(Values[2])]);
      3: result := format('Battle Global Script #%d', [Values[1]]);
   end;
   result := 'Call Script: ' + result;
end;

function TEBCallEvent.GetScriptText: string;
begin
   case Values[0] of
      0: result := format('globalScript%.4d;', [Values[1]]);
      1:
      begin
         if Values[1] <> 10005 then
            result := format('callScript(%d, %d);', [Values[1], Values[2]])
         else
            result := format('callScript(thisEvent.id, %d);', [Values[2]]);
      end;
      2: result := format('callScript(Ints[%d], Ints[%d]);', [Values[1], Values[2]]);
      3: result := format('battleGlobal%.4d;', [Values[1]]);
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

initialization
   TEBObject.RegisterClasses([TEBWait, TEBGlobalSwitch, TEBGlobalInt, TEBTimer,
                    TEBInput, TEBDeleteObj, TEBCallEvent, TEBGameOver,
                    TEBTitleScreen, TEBLocalSwitch]);
end.
