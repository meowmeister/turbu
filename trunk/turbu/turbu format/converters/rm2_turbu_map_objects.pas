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

unit rm2_turbu_map_objects;

interface
uses
   Classes,
   events, turbu_map_objects, move_data, turbu_pathing, EB_RpgScript;

type
   TScriptCallback = reference to procedure(script: TEBProcedure);

   T2k2RpgMapObject = class helper for TRpgMapObject
   public
      constructor Convert(base: TEvent; list: TStringList; saveScript: TScriptCallback);
   end;

   T2k2RpgEventPage = class helper for TRpgEventPage
   private
      procedure CalculateTileID(tile: word);
      procedure CreateEventScript(base: TEventPage; saveScript: TScriptCallback);
   public
      constructor Convert(base: TEventPage; id: integer; parent: TRpgMapObject;
        saveScript: TScriptCallback);
   end;

   T2k2RpgEventConditions = class helper for TRpgEventConditions
   private
      function convertConditionals(base: events.TPageConditions): TPageConditionSet;
   public
      constructor Convert(base: TEventConditions);
   end;

   T2k2Path = class helper for TPath
   private
      class function CalculateParamList(const rec: TMoveRecord): string;
      class function CalculateMoveCommand(const base: TMoveOrder; var index: integer): string;
   public
      class function CalculateMoveBaseString(base: TMoveOrder): string;
      constructor Convert(base: TEventMoveBlock);
   end;

   function ConvertEventScript(base: TEventCommandList; name: string): TEBProcedure;
   function ValidIdent(name: AnsiString): string;

implementation
uses
   SysUtils, Types, Generics.Collections,
   charset_data, rm2_turbu_maps, rm2_turbu_event_builder, turbu_operators,
   EventBuilder, EB_GotoRemoval, EB_NestedIfOptimization, EB_MessagePromptCollapser;

{ T2k2RpgMapObject }

constructor T2k2RpgMapObject.Convert(base: TEvent; list: TStringList; saveScript: TScriptCallback);
var
  I: Integer;
  lName: string;
begin
   inherited Create;
   FId := base.id;
   //ensure that each map object has a unique name across this map.  This keeps
   //EB procedure object names unique
   lName := ValidIdent(base.name);
   FName := lName;
   i := 0;
   while list.indexOf(FName) <> -1 do
   begin
      inc(i);
      FName := lName + intToStr(i);
   end;
   list.add(FName);
   location := base.location;
   for I := 1 to base.len do
      AddPage(TRpgEventPage.Convert(base.page[i - 1], i - 1, self, saveScript));
end;

class function T2k2Path.CalculateMoveBaseString(base: TMoveOrder): string;
const
   SEM = '; ';
var
   i: Integer;
begin
   result := '';
   i := 0;
   while i <= base.last do
      result := result + CalculateMoveCommand(base, i) + SEM;
   if result <> '' then
      delete(result, length(result), 1);
end;

class function T2k2Path.CalculateMoveCommand(const base: TMoveOrder; var index: integer): string;
var
   next: string;
   rec: TMoveRecord;
   op: byte;
   iterations: integer;
begin
   rec := base.command[index];
   op := rec.opcode;
   next := MOVE_CODES[op];
   inc(index);
   iterations := 1;
   if rec.opcode = $22 then
   begin
      next := QuotedStr(format('%s %d', [rec.name, rec.data[1]]));
      Exit(format('ChangeSprite(%s, 0)', [next]));
   end
   else if rec.opcode in CODES_WITH_PARAMS then
      Exit(format('%s(%s)', [next, CalculateParamList(rec)]))
   else while (index < base.last) and (base.command[index].opcode = op) do
   begin
      inc(iterations);
      inc(index);
   end;
   if iterations > 1 then
      result := format('%s(%d)', [next, iterations])
   else result := next;
end;

class function T2k2Path.CalculateParamList(const rec: TMoveRecord): string;
const
   ADD_INT = '%s, %d';
var
   i: integer;
begin
   case rec.opcode of
      $20, $21: result := intToStr(rec.data[1]);
      $22:
      begin
         result := QuotedStr(rec.name);
         result := format(ADD_INT, [result, rec.data[1]]);
      end;
      $23:
      begin
         result := QuotedStr(rec.name);
         for i := 1 to 3 do
            result := format(ADD_INT, [result, rec.data[i]]);
      end;
   end;
end;

constructor T2k2Path.Convert(base: TEventMoveBlock);
begin
   if assigned(base.moveBlock) then
      self.Create(CalculateMoveBaseString(base.moveBlock), base.moveBlock.loop)
   else self.Create;
end;

function ValidIdent(name: AnsiString): string;
var
   i: integer;
begin
   setLength(result, length(name));
   for i := 1 to length(name) do
      if name[i] in ['A'..'Z', 'a'..'z', '_', '0'..'9'] then
         result[i] := Char(name[i])
      else result[i] := '_';
   if (result = '') or (not (name[1] in ['A'..'Z', 'a'..'z'])) then
      result := 'O' + result;
end;

{ T2k2RpgEventPage }

const WHICH_TILE: array[0..2] of word = (1, 0, 2);

procedure T2k2RpgEventPage.CalculateTileID(tile: word);
var
   dec: TDecodeResult;
begin
   dec := decode(tile + 10000);
   self.whichTile := dec.baseTile;
   FName := '*' + intToStr(dec.group + 10); //* character can't be used in filenames
end;

constructor T2k2RpgEventPage.Convert(base: TEventPage; id: integer;
  parent: TRpgMapObject; saveScript: TScriptCallback);
begin
   inherited Create(parent, id);
   FConditions.Convert(base.conditionBlock);
   if base.filename = '' then
      CalculateTileID(base.whichChip)
   else begin
      FName := format('%s %d', [string(base.filename), base.whichChip]);
      self.whichTile := WHICH_TILE[ord(base.animFrame)];
   end;
   self.direction := base.direction;
   Ftransparent := base.transparent;
   if assigned(base.moveBlock) then
   begin
      self.path.Free;
      self.path := TPath.Convert(base.moveBlock);
   end;
   self.moveType := TMoveType(base.moveType);
   self.moveFrequency := base.moveFrequency;
   self.startCondition := TStartCondition(base.startCondition);
   self.zOrder := base.zOrder;
   self.isBarrier := base.is_barrier;
   self.animType := TAnimType(base.animType);
   self.moveSpeed := base.moveSpeed;
   if base.opcode.count > 1 then //a blank script has a single "end" opcode
      CreateEventScript(base, saveScript);
end;

procedure T2k2RpgEventPage.CreateEventScript(base: TEventPage; saveScript: TScriptCallback);
const
   PROCNAME = '%s_page%d';
var
   scriptname: string;
begin
   scriptname := format(PROCNAME, [self.parent.name, self.id + 1]);
   FEventText := scriptname;
   saveScript(ConvertEventScript(base.opcode, scriptname));
end;

{ T2k2RpgEventConditions }

constructor T2k2RpgEventConditions.Convert(base: TEventConditions);
begin

   self.conditions := convertConditionals(base.conditions);
   self.switch1Set := base.switch1Set;
   self.switch2Set := base.switch2Set;
   self.variable1Set := base.variableSet;
   self.variable1Value := base.variableValue;
   self.variable1Op := TComparisonOp(base.varOperator);
   self.itemNeeded := base.itemNeeded;
   self.heroNeeded := base.heroNeeded;
   self.timeRemaining := base.timeRemaining;
   self.timeRemaining2 := base.clock2;
   self.timer1Op := co_ltE;
   self.timer2Op := co_ltE;
end;

function T2k2RpgEventConditions.convertConditionals(base: events.TPageConditions): TPageConditionSet;
begin
   result := [];
   if base[switch1] then
      include(result, pc_switch1);
   if base[switch2] then
      include(result, pc_switch2);
   if base[variable1] then
      include(result, pc_var1);
   if base[item] then
      include(result, pc_item);
   if base[hero] then
      include(result, pc_hero);
   if base[timer] then
      include(result, pc_timer1);
   if base[timer2] then
      include(result, pc_timer2);
end;

function ConvertEventScript(base: TEventCommandList; name: string): TEBProcedure;
const
   REMFUDGE: array[1..5] of integer = (20141, 20151, 20713, 20722, 20732);
var
   command: TEventCommand;
   new, last: TEBObject;
   stack: TStack<TEBObject>;
   fudgeFactor, idx: integer;
   converter: TScriptConverter;
begin
   fudgeFactor := 0;
   stack := TStack<TEBObject>.Create;
   result := TEBProcedure.Create(nil);
   result.name := name;
   converter := TScriptConverter.Create;
   try
   try
      last := result;
      for command in base do
      begin
         if TArray.BinarySearch<integer>(REMFUDGE, command.opcode, idx) then
            dec(fudgeFactor);
         if command.indent + fudgeFactor >= stack.Count then
            stack.Push(last)
         else if command.indent + fudgeFactor < stack.Count - 1 then
            stack.Pop;
         if (command.opcode = 20110) or (command.opcode = 22410) then //additional message line
            converter.ConvertOpcode(command, last)
         else begin
            new := converter.ConvertOpcode(command, stack.Peek);
            if assigned(new) then
               last := new
            else last := stack.peek;
         end;
         if (command.opcode = 10140) or
            ((command.opcode = 10710) and ((command.data[3] = 2) or (command.data[4] <> 0))) or
            (((command.opcode = 10720) or (command.opcode = 10730)) and (command.data[2] <> 0)) then
            inc(fudgeFactor);
      end;
      assert(stack.Count = 1);
      assert(fudgeFactor = 0);
      NestedIfOptimization(result);
      if converter.LabelGotoCount > 0 then
      begin
         GotoRemoval(result);
         NestedIfOptimization(result);
      end;
      ChoiceMerge(result);
      InputMerge(result);
   except
      result.SaveScript;
      result.free;
      raise;
   end;
   finally
      stack.Free;
      converter.Free;
   end;
end;

end.
