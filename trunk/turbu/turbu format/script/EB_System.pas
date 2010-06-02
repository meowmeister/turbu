unit EB_System;

interface
uses
   EventBuilder, EB_Expressions, turbu_defs;

type
   TEBWait = class(TEBObject)
   public
      function GetNodeText: string; override;
      function GetScript(indent: integer): string; override;
   end;

   TEBGlobalSwitch = class(TEBObject)
   public
      constructor Create(parent: TEBObject; switch: TEBExpression; op: integer); reintroduce;
      function GetNodeText: string; override;
      function GetScript(indent: integer): string; override;
   end;

   TEBGlobalInt = class(TEBObject)
   public
      constructor Create(parent: TEBObject; int: TEBExpression; value: TEBExpression); reintroduce;
      function GetNodeText: string; override;
      function GetScript(indent: integer): string; override;
   end;

   TEBTimer = class(TEBObject)
   public
      function GetNodeText: string; override;
      function GetScript(indent: integer): string; override;
   end;

   TEBMoney = class(TEBObject)
   public
      function GetNodeText: string; override;
      function GetScript(indent: integer): string; override;
   end;

   TEBInventory = class(TEBObject)
   public
      function GetNodeText: string; override;
      function GetScript(indent: integer): string; override;
   end;

   TEBChangeParty = class(TEBObject)
   public
      function GetNodeText: string; override;
      function GetScript(indent: integer): string; override;
   end;

implementation
uses
   SysUtils, Classes,
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
   result := 'Wait ' + FormatFloat('###.#', Values[0] / 10) + ' secs';
end;

function TEBWait.GetScript(indent: integer): string;
const
   LINE = 'Wait(%d);';
begin
   result := IndentString(indent) + format(LINE, [Values[0]]);
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

function TEBGlobalSwitch.GetScript(indent: integer): string;
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

function TEBGlobalInt.GetScript(indent: integer): string;
const
   LINE = 'Ints[%s] := %s';
var
   int, value: TEBExpression;
begin
   int := self.components[0] as TEBExpression;
   value := self.components[1] as TEBExpression;
   result := IndentString(indent) + format(LINE, [int.GetScript(0), value.GetScript(0)]);
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

function TEBTimer.GetScript(indent: integer): string;
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
   result := IndentString(indent) + result;
end;

{ TEBMoney }

function TEBMoney.GetNodeText: string;
const
   LINE = 'Change Money: %s $%s';
   SIGNS: array[0..1] of string = ('Add', 'Subtract');
begin
   result := format(LINE, [SIGNS[Values[0]], GetIntScript(values[1], values[2])]);
end;

function TEBMoney.GetScript(indent: integer): string;
const
   LINE = 'money := money %s %s;';
   SIGNS: array[0..1] of char = ('+', '-');
begin
   result := IndentString(indent) + format(LINE, [SIGNS[Values[0]], GetIntScript(values[1], values[2])]);
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

function TEBInventory.GetScript(indent: integer): string;
const LINE = '%sItem(%s, %s);';
begin
   result := indentString(indent) + format(LINE, [ADDREM[Values[0]],
                                           GetIntScript(Values[1], Values[2]),
                                           (Components[0] as TEBExpression).GetScript(0)]);
end;

{ TEBChangeParty }

function TEBChangeParty.GetNodeText: string;
const LINE = 'Change Party: %s %s';
begin
   result := format(LINE, [ADDREM[values[0]], (Components[0] as TEBExpression).GetNodeText]);
end;

function TEBChangeParty.GetScript(indent: integer): string;
var
   param: string;
begin
   param := (Components[0] as TEBExpression).GetScript(0);
   if boolean(Values[0]) then
      result := format('heroLeave(%s);', [param])
   else result := format('heroJoin(%s);', [param]);
   result := IndentString(indent) + result;
end;

initialization
   RegisterClasses([TEBWait, TEBGlobalSwitch, TEBGlobalInt, TEBTimer, TEBMoney,
                    TEBInventory]);
end.
