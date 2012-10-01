{ *****************************************************************************
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
  ***************************************************************************** }

unit rm2_turbu_event_builder;

interface
uses
   Classes,
   Events, EventBuilder;

function ConvertOpcode(opcode: TEventCommand; parent: TEBObject): TEBObject;
function UnknownOpcodeList: TStringList;

implementation
uses
   SysUtils, Generics.Defaults, Generics.Collections, TypInfo,
   commons, move_data, turbu_pathing, rm2_turbu_map_objects, formats,
   EB_RpgScript, EB_Messages, EB_Expressions, EB_System, EB_Maps, EB_Characters,
   EB_Settings, EB_Media, EB_Battle, EB_Expressions_RM, turbu_battle_engine, turbu_operators;

type
   TConvertRoutine = function (opcode: TEventCommand; parent: TEBObject): TEBObject;

   TOpcodePair = record
      K: integer;
      V: TEBClass;
   end;

   TComplexOpcodePair = record
      K: integer;
      V: TConvertRoutine;
   end;

   TOpcodeDictionary = TDictionary<integer, TEBClass>;
   TComplexOpcodeDictionary = TDictionary<integer, TConvertRoutine>;
   TUnknownOpcodeDictionary = TDictionary<integer, integer>;
   TIntPair = TPair<integer, integer>;
   TPartySubscript = reference to function(subscript: TEBChainable; out expr: TEBExpression): TEBObject;

var
   dict: TOpcodeDictionary;
   cDict: TComplexOpcodeDictionary;
   blockStack: TStack<integer>;
   ifStack: TStack<TEBIf>;
   tally: TUnknownOpcodeDictionary;
   LoopDepth: integer;

const
   APPEND = '%s'#13#10'%s';

function ComparePairs(const Left, Right: TIntPair): Integer;
begin
  result := right.Value - left.Value;
end;

function UnknownOpcodeList: TStringList;
var
   pair: TPair<integer, integer>;
   list: TList<TIntPair>;
begin
   list := TList<TIntPair>.Create;
   try
      for pair in tally do
         list.add(pair);
      list.Sort(TComparer<TIntPair>.Construct(ComparePairs));
      result := TStringList.Create;
      try
         for pair in list do
            result.Add(format('Opcode %d used %d times.', [pair.Key, pair.Value]));
      except
         result.Free;
         raise;
      end;
   finally
      list.free;
   end;
end;

procedure LogUnknownOpcode(opcode: integer);
var
   count: integer;
begin
   if not tally.TryGetValue(opcode, count) then
      count := 0;
   inc(count);
   tally.AddOrSetValue(opcode, count);
end;

function ObjectFactory(newClass: TEBClass; opcode: TEventCommand; parent: TEBObject): TEBObject;
begin
   result := newClass.Create(parent);
   result.Text := StringReplace(StringReplace(string(opcode.name), chr(13), '', [rfReplaceAll]), chr(10), '', [rfReplaceAll]);
   result.Values.AddRange(opcode.data);
end;

function CreateForLoop(parent, child: TEBObject; lbound, ubound: integer): TEBForLoop;
begin
   result := TEBForLoop.Create(parent);
   result.add(child);
   result.Values.add(lbound);
   result.Values.add(ubound);
   result.text := 'Num';
end;

function SetupPartySubscript(mode, value, data: integer; work: TPartySubscript): TEBObject;
var
   subscript: TEBChainable;
   expr: TEBExpression;
begin
   case mode of
      0: subscript := TEBObjArrayValue.Create('Party', TEBVariableValue.Create('Num'));
      1: subscript := TEBLookupObjExpr.Create('Hero', value, 'heroes');
      2: subscript := TEBLookupObjExpr.Create('Hero', TEBIntsValue.Create(value), 'heroes');
      else raise ERPGScriptError.CreateFmt('Unknown subscript data value: %d!', [mode]);
   end;
   result := work(subscript, expr);
   result.values.insert(0, data);
   result.add(subscript);
   if assigned(expr) then
      result.add(expr);
   if mode = 0 then
      result := CreateForLoop(TEBObject(result.owner), result, 1, 4);
end;

function ConvertOpcode(opcode: TEventCommand; parent: TEBObject): TEBObject;
var
   newClass: TEBClass;
   converter: TConvertRoutine;
begin
   if cDict.TryGetValue(opcode.opcode, converter) then
      Exit(converter(opcode, parent))
   else if not dict.TryGetValue(opcode.opcode, newClass) then
      newClass := TEbUntranslated;
   if assigned(newClass) then
   begin
      result := objectFactory(newClass, opcode, parent);
      if newClass = TEbUntranslated then
      begin
         TEbUntranslated(result).opcode := opcode.opcode;
         LogUnknownOpcode(opcode.opcode);
      end;
   end
   else result := nil;
end;

function ConvertMessageOptions(opcode: TEventCommand; parent: TEBObject): TEBObject;
begin
   result := objectFactory(TEBMessageOptions, opcode, parent);
   result.Values[0] := result.Values[0] xor 1;
   result.Values[3] := result.Values[3] xor 1;
end;

function ConvertCase(opcode: TEventCommand; parent: TEBObject): TEBObject;
begin
   result := objectFactory(TEBChoiceMessage, opcode, parent);
   blockStack.Push(0);
end;

function ConvertEndCase(opcode: TEventCommand; parent: TEBObject): TEBObject;
begin
   assert(blockStack.Pop >= 0);
   result := objectFactory(TEBEndCase, opcode, parent);
end;

function ConvertCaseExtension(opcode: TEventCommand; parent: TEBObject): TEBObject;
var
   index: integer;
begin
   if opcode.name = '' then
   begin
      assert(blockStack.Peek >= 0);
      result := objectFactory(TEBElseBlock, opcode, parent);
   end
   else
   begin
      index := blockStack.Pop;
      try
         assert(index >= 0);
         result := objectFactory(TEBCaseBlock, opcode, parent);
         result.Values.Add(index);
      finally
         blockStack.Push(index + 1);
      end;
   end;
end;

//this should be part of ConvertIf, but there was enough work involved to extract
//it into its own routine for readability purposes.
procedure PrepareHeroIf(d2, d3: integer; name: string; out left, right: TEBExpression; out op: TComparisonOp);
begin
   right := nil;
   case d2 of
      0:
      begin
         left := TEBPropExpr.Create('InParty');
         TEBPropExpr(left).hint := 2;
      end;
      1:
      begin
         left := TEBPropExpr.Create('name');
         right := TEBStringValue.Create(name);
      end;
      2:
      begin
         left := TEBPropExpr.Create('level');
         right := TEBIntegerValue.Create(d3);
      end;
      3:
      begin
         left := TEBPropExpr.Create('HP');
         right:= TEBIntegerValue.Create(d3);
      end;
      4: left := TEBObjArrayValue.Create('skill', d3);
      5:
      begin
         left := TEBCall.Create('Equipped');
         left.Add(TEBLookupValue.Create(d3, 'items'));
         TEBCall(left).hint := 3;
      end;
      6: left := TEBLookupObjExpr.Create('condition', d3, 'conditions');
   end;

   if d2 in [2, 3] then
      op := co_gtE
   else op := co_equals;
end;

function eventDeref(const data: integer): TEBObjExpr;
begin
   case data of
      10001: result := TEBObjExpr.Create('Party');
      10002..10004: result := TEBLookupObjExpr.Create('vehicle', data - 10001, 'vehicles');
      10005: result := TEBObjExpr.Create('ThisObject');
      else result := TEBObjArrayValue.Create('MapObject', data);
   end;
end;

function ConvertIf(opcode: TEventCommand; parent: TEBObject): TEBObject;
const
   FACING_VALUES: array[0..3] of string = ('facing_up', 'facing_right', 'facing_down', 'facing_left');
var
   left, right: TEBExpression;
   op: TComparisonOp;
begin
   op := co_equals;
   right := nil;
   case opcode.data[0] of
      0:
      begin
         left := TEBSwitchesValue.Create(opcode.data[1]);
         right := TEBBooleanValue.Create(not boolean(opcode.data[2]));
      end;
      1:
      begin
         left := TEBIntsValue.Create(opcode.data[1]);
         if boolean(opcode.data[2]) then
            right := TEBIntsValue.Create(opcode.data[3])
         else right := TEBIntegerValue.Create(opcode.data[3]);
         op := TComparisonOp(opcode.data[4]);
      end;
      2, 3, 10:
      begin
         if opcode.Data[0] = 2 then
            left := TEBObjExpr.Create('timer', TEBPropExpr.Create('time'))
         else if opcode.data[0] = 3 then
            left := TEBObjExpr.Create('party', TEBPropExpr.Create('money'))
         else left := TEBObjExpr.Create('timer2', TEBPropExpr.Create('time'));
         if opcode.Data[2] = 0 then
            op := co_gte
         else op := co_lte;
         right := TEBIntegerValue.Create(opcode.Data[1]);
      end;
      4:
      begin
         left := TEBCall.Create('Contains');
         left.Add(TEBLookupValue.Create(opcode.data[1], 'items'));
         left := TEBObjExpr.Create('party', TEBPropExpr.Create('inventory', TEBCall(left)));
         right := TEBBooleanValue.Create(not boolean(opcode.data[2]));
      end;
      5:
      begin
         PrepareHeroIf(opcode.Data[2], opcode.Data[3], string(opcode.name), left, right, op);
         left := TEBLookupObjExpr.Create('hero', opcode.Data[1], 'heroes', left as TEBChainable);
      end;
      6:
      begin
         right := TEBEnumValue.Create(FACING_VALUES[opcode.data[2]]);
         left := EventDeref(opcode.data[1]);
         TEBChainable(left).chain := TEBPropExpr.Create('facing');
      end;
      7:
      begin
         left := eventDeref(opcode.data[1] + 10002);
         TEBChainable(left).chain := TEBPropExpr.Create('InUse');
      end;
      8: left := TEBVariableValue.Create('StartedWithButton');
      9: left := TEBObjExpr.Create('bgm', TEBPropExpr.Create('looped'));
      else raise ERpgScriptError.CreateFmt('Unknown data[0] value %d', [opcode.data[0]]);
   end;
   if right = nil then
      right := TEBBooleanValue.Create(true);
   result := TEBIf.Create(parent, left, right, op);
   blockStack.push(-1);
   ifStack.Push(TEBIf(result));
end;

function ConvertBattleIf(opcode: TEventCommand; parent: TEBObject): TEBObject;
var
   left: TEBExpression;
begin
   case opcode.data[0] of
      0..1: Exit(ConvertIf(opcode, parent));
      2: left := TEBLookupObjExpr.Create('hero', opcode.Data[1], 'heroes', TEBPropExpr.Create('CanAct'));
      3: left := TEBLookupObjExpr.Create('monster', opcode.Data[1], 'monsters', TEBPropExpr.Create('CanAct'));
      4: left := TEBLookupObjExpr.Create('monster', opcode.Data[1], 'monsters', TEBPropExpr.Create('Targetted'));
      5: begin
         left := TEBCall.Create('UsesCommand');
         left.Add(TEBLookupValue.Create(opcode.data[2], 'commands'));
         left := TEBLookupObjExpr.Create('hero', opcode.Data[1], 'heroes', TEBChainable(left));
      end;
      else raise ERpgScriptError.CreateFmt('Unknown data[0] value %d', [opcode.data[0]]);
   end;
   result := TEBIf.Create(parent, left, TEBBooleanValue.Create(true), co_equals);
   blockStack.push(-1);
   ifStack.Push(TEBIf(result));
end;

function ConvertIfElse(opcode: TEventCommand; parent: TEBObject): TEBObject;
begin
   assert(blockStack.peek = -1);
   ifStack.Peek.SetElse;
   result := ifStack.Peek;
end;

function ConvertEndIf(opcode: TEventCommand; parent: TEBObject): TEBObject;
begin
   assert(blockStack.pop = -1);
   ifStack.pop;
   result := nil;
end;

function ConvertBattle(opcode: TEventCommand; parent: TEBObject): TEBObject;
var
   block: TEBEnumCaseBlock;
   battleResults: TBattleResultSet;
begin
   if high(opcode.data) > 5 then
      result := TEBBattleEx.Create(parent)
   else result := TEBBattle.Create(parent);
   result.Values.Add(opcode.data[0]);
   result.Values.Add(opcode.data[1]);
   if opcode.data[2] = 1 then
      result.Text := string(opcode.name);
   if high(opcode.data) > 5 then
   begin
      if opcode.data[5] = 1 then
         result.Values.Add(5)
      else result.Values.Add(opcode.data[6]);
      result.Values.Add(opcode.data[2]);
      case opcode.data[2] of
         0: result.Values.Add(0);
         1: result.Values.Add(opcode.data[7] - 1);
         2: result.Values.Add(opcode.data[8]);
      end;
   end
   else result.Values.Add(opcode.data[5]);

   battleResults := [];
   (result as TEBMaybeCase).CaseBlock := (opcode.Data[3] <> 0) or (opcode.Data[4] <> 0);
   if (opcode.Data[3] > 0) then
   begin
      include(battleResults, br_escaped);
      if opcode.data[3] = 1 then
      begin
         block := TEBEnumCaseBlock.Create(result);
         block.Text := 'br_escaped';
         block.Add(TEBExit.Create(nil));
      end;
   end;
   if (boolean(opcode.Data[4]) = true) then
      include(battleResults, br_defeated);
   (result as TEBBattleBase).results := battleResults;
end;

function ConvertVictory(opcode: TEventCommand; parent: TEBObject): TEBObject;
begin
   result := TEBEnumCaseBlock.Create(parent);
   result.text := 'br_victory';
end;

function ConvertEscape(opcode: TEventCommand; parent: TEBObject): TEBObject;
begin
   result := TEBEnumCaseBlock.Create(parent);
   result.text := 'br_escaped';
end;

function ConvertDefeat(opcode: TEventCommand; parent: TEBObject): TEBObject;
begin
   result := TEBEnumCaseBlock.Create(parent);
   result.text := 'br_defeated';
end;

function ConvertSwitch(opcode: TEventCommand; parent: TEBObject): TEBObject;
//in RPG Maker, 1 = false, 0 = true for this opcode.  Transposing for sanity's sake.
const
   TRANSPOSE: array[0..2] of integer = (1, 0, 2);
var
   val: TEBExpression;
begin
   val := CreateSubscript(opcode.data[0], opcode.data[1]);
   if not IsBetween(opcode.data[3], 0, 3) then
      raise ERPGScriptError.CreateFmt('Unknown switch data 3 value: %d!', [opcode.Data[3]]);
   result := TEBGlobalSwitch.Create(parent, val, TRANSPOSE[opcode.data[3]]);
   if opcode.data[0] = 1 then
      result := CreateForLoop(parent, result, opcode.data[1], opcode.data[2]);
end;

function ConvertVar(opcode: TEventCommand; parent: TEBObject): TEBObject;
const
   OPS: array[1..5] of TBinaryOp = (bo_add, bo_sub, bo_mult, bo_div, bo_mod);
   HEROPROPS: array[0..5] of string = ('level', 'exp', 'hp', 'mp', 'maxHP', 'maxMP');
   EVENTPROPS: array[0..5] of string = ('MapID', 'xPos', 'yPos', 'facingValue', 'ScreenX', 'ScreenY');
   MISCPROPS: array[0..9] of string = ('money', 'timer.time', 'partySize', 'saveCount',
                                       'battleCount', 'victories', 'losses', 'flees',
                                       'bgm.position', 'timer2.time');
var
   lValue, rValue: TEBExpression;
   propname: string;
begin
   lValue := CreateSubscript(opcode.data[0], opcode.data[1]);
   //create bin2
   case opcode.Data[4] of
      0: rValue := TEBIntegerValue.Create(opcode.Data[5]);
      1: rValue := TEBIntsValue.Create(opcode.Data[5]);
      2: rValue := TEBIntsValue.Create(TEBIntsValue.Create(opcode.Data[5]));
      3:
      begin
         rValue := TEBCall.Create('random');
         rValue.add(TEBIntegerValue.Create(opcode.data[5]));
         rValue.add(TEBIntegerValue.Create(opcode.data[6]));
      end;
      4:
      begin
         rValue := TEBCall.Create('heldItems');
         rValue.add(TEBIntegerValue.Create(opcode.data[5]));
         rValue.add(TEBBooleanValue.Create(boolean(opcode.data[6])));
      end;
      5:
      begin
         rValue := TEBLookupObjExpr.Create('hero', opcode.data[5], 'heroes');
         case opcode.Data[6] of
            0..5: propname := HEROPROPS[opcode.data[6]];
            6..9: propname := format('stat[%d]', [opcode.Data[6] - 5]);
            10..14: propname := format('equipment[%d]', [opcode.Data[6] - 9]);
            else raise ERPGScriptError.CreateFmt('Unknown variable data 6 value: %d!', [opcode.Data[6]]);
         end;
         TEBChainable(rValue).chain := TEBPropExpr.Create(propname);
      end;
      6:
      begin
         rValue := eventDeref(opcode.Data[5]);
         TEBChainable(rValue).Chain := TEBPropExpr.Create(EVENTPROPS[opcode.data[6]]);
      end;
      7: rValue := TEBVariableValue.Create(MISCPROPS[opcode.Data[5]]);
      8:
      begin
         rValue := TEBLookupObjExpr.Create('monster', opcode.data[5], 'monsters');
         case opcode.Data[6] of
            0..3: propname := HEROPROPS[opcode.data[6] + 2];
            4..7: propname := format('stat[%d]', [opcode.Data[6] - 4]);
            else raise ERPGScriptError.CreateFmt('Unknown variable data 6 value: %d!', [opcode.Data[6]]);
         end;
         TEBChainable(rValue).chain := TEBPropExpr.Create(propname);
      end;
      else raise ERPGScriptError.CreateFmt('Unknown variable data 4 value: %d!', [opcode.Data[4]]);
   end;
   if opcode.data[3] > 0 then
      rValue := TEBBinaryOp.Create(TEBIntsValue.Create(CreateSubscript(opcode.data[0], opcode.data[1])),
                                   rValue, OPS[opcode.data[3]]);
   result := TEBGlobalInt.Create(parent, lValue, rValue);
   if opcode.data[0] = 1 then
      result := CreateForLoop(parent, result, opcode.data[1], opcode.data[2]);
end;

function ConvertInventory(opcode: TEventCommand; parent: TEBObject): TEBObject;
var
   expr: TEBExpression;
begin
   result := TEBInventory.Create(parent);
   result.Values.AddRange([opcode.Data[0], opcode.Data[1], opcode.Data[4]]);
   if boolean(opcode.data[3]) then
      expr := TEBIntsValue.Create(opcode.Data[2])
   else expr := TEBLookupValue.Create(opcode.Data[2], 'Items');
   result.add(expr);
end;

function ConvertParty(opcode: TEventCommand; parent: TEBObject): TEBObject;
var
   expr: TEBExpression;
begin
   result := TEBChangeParty.Create(parent);
   result.Values.Add(opcode.Data[0]);
   if boolean(opcode.data[1]) then
      expr := TEBIntsValue.Create(opcode.Data[2])
   else expr := TEBLookupValue.Create(opcode.Data[2], 'Heroes');
   result.add(expr);
end;

function ConvertStats(opcode: TEventCommand; parent: TEBObject): TEBObject;
begin
   result := SetupPartySubscript(Opcode.data[0], Opcode.data[1], Opcode.data[2],
      function(subscript: TEBChainable; out expr: TEBExpression): TEBObject
      begin
         subscript.Chain := TEBPropExpr.Create(STATS[opcode.data[3]]);
         subscript.Chain.hint := 1;
         result := TEBStats.Create(parent);
         if boolean(Opcode.data[4]) then
            expr := TEBIntsValue.Create(Opcode.data[5])
         else expr := TEBIntegerValue.Create(Opcode.data[5]);
      end);
end;

function ConvertSkills(opcode: TEventCommand; parent: TEBObject): TEBObject;
begin
   result := SetupPartySubscript(Opcode.data[0], Opcode.data[1], Opcode.data[2],
      function(subscript: TEBChainable; out expr: TEBExpression): TEBObject
      begin
         if boolean(opcode.Data[3]) then
            expr := TEBIntsValue.Create(opcode.Data[4])
         else expr := TEBIntegerValue.Create(opcode.Data[4]);
         result := TEBSkills.Create(parent);
      end);
end;

function ConvertEquipment(opcode: TEventCommand; parent: TEBObject): TEBObject;
begin
   result := SetupPartySubscript(Opcode.data[0], Opcode.data[1], Opcode.data[2],
      function(subscript: TEBChainable; out expr: TEBExpression): TEBObject
      begin
         if boolean(opcode.Data[2]) then
            expr := TEBEnumValue.Create(SLOTS[opcode.Data[3]])
         else if boolean(opcode.data[3]) then
            expr := TEBIntsValue.Create(opcode.Data[4])
         else expr := TEBLookupValue.Create(opcode.Data[4], 'items');
         result := TEBEquipment.Create(parent);
      end);
end;

function ConvertHP(opcode: TEventCommand; parent: TEBObject): TEBObject;
begin
   result := SetupPartySubscript(Opcode.data[0], Opcode.data[1], Opcode.data[2],
      function(subscript: TEBChainable; out expr: TEBExpression): TEBObject
      begin
         if boolean(opcode.data[3]) then
            expr := TEBIntsValue.Create(opcode.Data[4])
         else expr := TEBIntegerValue.Create(opcode.Data[4]);
         result := TEBChangeHP.Create(parent);
         result.Values.Add(opcode.data[5]);
      end);
end;

function ConvertMP(opcode: TEventCommand; parent: TEBObject): TEBObject;
begin
   result := SetupPartySubscript(Opcode.data[0], Opcode.data[1], Opcode.data[2],
      function(subscript: TEBChainable; out expr: TEBExpression): TEBObject
      begin
         if boolean(opcode.data[3]) then
            expr := TEBIntsValue.Create(opcode.Data[4])
         else expr := TEBIntegerValue.Create(opcode.Data[4]);
         result := TEBChangeMP.Create(parent);
      end);
end;

function ConvertStatus(opcode: TEventCommand; parent: TEBObject): TEBObject;
begin
   result := SetupPartySubscript(Opcode.data[0], Opcode.data[1], Opcode.data[2],
      function(subscript: TEBChainable; out expr: TEBExpression): TEBObject
      begin
         expr := TEBLookupValue.Create(opcode.Data[3], 'conditions');
         result := TEBChangeStatus.Create(parent);
      end);
end;

function ConvertFullHeal(opcode: TEventCommand; parent: TEBObject): TEBObject;
begin
   result := SetupPartySubscript(Opcode.data[0], Opcode.data[1], 0,
      function(subscript: TEBChainable; out expr: TEBExpression): TEBObject
      begin
         expr := nil;
         result := TEBFullHeal.Create(parent);
      end);
end;

function ConvertSetSprite(opcode: TEventCommand; parent: TEBObject): TEBObject;
begin
   result := TEBHeroSprite.Create(parent);
   result.Text := format('%s %d', [opcode.name, opcode.Data[1]]);
   result.Values.AddRange([opcode.Data[0], opcode.Data[2]]);
end;

function ConvertSetPortrait(opcode: TEventCommand; parent: TEBObject): TEBObject;
begin
   result := ObjectFactory(TEBHeroPortrait, opcode, parent);
   result.Values[1] := result.Values[1] + 1;
end;

function ConvertVSprite(opcode: TEventCommand; parent: TEBObject): TEBObject;
begin
   result := TEBVehicleSprite.Create(parent);
   result.Text := format('%s %d', [opcode.name, opcode.Data[1]]);
   result.Values.Add(opcode.Data[0]);
   result.Values.Add(0);
end;

procedure SetupMif(mif: TEBMaybeIf; opcode: TEventCommand; const blockName: string);
begin
   mif.IfBlock := boolean(opcode.data[2]);
   mif.Setup(blockName);
   if boolean(opcode.data[2]) then
   begin
      blockStack.push(-1);
      ifStack.Push(mif);
   end;
end;

function ConvertShop(opcode: TEventCommand; parent: TEBObject): TEBObject;
var
   i: integer;
begin
   result := TEBShop.Create(parent);
   result.Values.Add(opcode.data[0]);
   result.Values.Add(opcode.data[1]);
   assert(opcode.data[3] = 0);
   for I := 4 to high(opcode.data) do
      result.values.add(opcode.data[i]);
   SetupMif(result as TEBMaybeIf, opcode, 'Transaction');
end;

function ConvertInn(opcode: TEventCommand; parent: TEBObject): TEBObject;
begin
   result := TEBInn.Create(parent);
   result.Values.Add(opcode.data[0]);
   result.Values.Add(opcode.data[1]);
   SetupMif(result as TEBMaybeIf, opcode, 'Stay');
end;

function ConvertTeleportVehicle(opcode: TEventCommand; parent: TEBObject): TEBObject;
begin
   result := TEBTeleportVehicle.Create(parent);
   result.Values.Add(opcode.data[0] + 1);
   result.Values.AddRange([opcode.data[1], opcode.data[2], opcode.data[3], opcode.data[4]]);
end;

function ConvertTeleportEvent(opcode: TEventCommand; parent: TEBObject): TEBObject;
begin
   result := TEBTeleportMapObj.Create(parent);
   result.Add(eventDeref(opcode.data[0]));
   result.Values.AddRange([opcode.data[1], opcode.data[2], opcode.data[3]]);
end;

function ConvertSwapObjects(opcode: TEventCommand; parent: TEBObject): TEBObject;
begin
   result := TEBSwapObjects.Create(parent);
   result.Add(eventDeref(opcode.data[0]));
   result.Add(eventDeref(opcode.data[1]));
end;

function Cleanup(opcode: TEventCommand; parent: TEBObject): TEBObject;
begin
   assert(blockStack.Count = 0);
   assert(ifStack.Count = 0);
   result := nil;
end;

function ConvertNewImage(opcode: TEventCommand; parent: TEBObject): TEBObject;
var
   effect: boolean;
   i: integer;
   sub: TEBObject;
begin
   result := TEBNewImage.Create(parent);
   result.Text := string(opcode.Name);
   result.Values.AddRange([opcode.data[0], opcode.data[1], opcode.data[2],
                          opcode.data[3], opcode.data[5], opcode.data[6],
                          opcode.data[4], opcode.data[7]]);
   effect := false;
   for I := 8 to 11 do
      effect := effect or (opcode.data[i] <> 100);
   if effect then
   begin
      sub := TEBImageColor.Create(result);
      sub.Values.AddRange([opcode.data[0], opcode.data[8], opcode.data[9],
                          opcode.data[10], opcode.data[11]]);
   end;
   if (opcode.data[12] <> 0) then
   begin
      sub := TEBImageEffect.Create(result);
      sub.Values.AddRange([opcode.data[0], opcode.data[12], opcode.data[13]]);
   end;
end;

function ConvertMoveImage(opcode: TEventCommand; parent: TEBObject): TEBObject;
var
   sub: TEBObject;
begin
   assert(opcode.Data[4] = 0);
   assert(opcode.Data[7] = 0);
   assert(high(opcode.Data) in [15, 16]);
   result := TEBImageMove.Create(parent);
   result.Text := string(opcode.Name);
   result.Values.AddRange([opcode.data[0], opcode.data[1], opcode.data[2],
                          opcode.data[3], opcode.data[5], opcode.data[6],
                          opcode.data[14], opcode.data[15]]);
   if high(opcode.data) = 16 then
      result.Values.Add(opcode.data[16])
   else result.Values.Add(-1);
   if (opcode.data[8] <> 0) or (opcode.data[9] <> 0) or (opcode.data[10] <> 0)
      or (opcode.data[11] <> 100) then
   begin
      sub := TEBImageColor.Create(result);
      sub.Values.AddRange([opcode.data[0], opcode.data[8], opcode.data[9],
                          opcode.data[10], opcode.data[11]]);
   end;
   if opcode.data[12] <> 0 then
   begin
      sub := TEBImageEffect.Create(result);
      sub.Values.AddRange([opcode.data[0], opcode.data[12], opcode.data[13]]);
   end;
end;

function ConvertShowAnim(opcode: TEventCommand; parent: TEBObject): TEBObject;
begin
   result := TEBShowAnim.Create(parent);
   result.Values.AddRange([opcode.Data[0], opcode.Data[2], opcode.Data[3]]);
   if boolean(opcode.Data[3]) then
      TEBNilValue.Create(result)
   else result.Add(eventDeref(opcode.Data[1]));
end;

function ConvertTranslucency(opcode: TEventCommand; parent: TEBObject): TEBObject;
begin
   result := TEBTranslucency.Create(parent);
   if boolean(opcode.Data[0]) then
      result.values.add(0)
   else result.values.add(30);
end;

function ConvertFlash(opcode: TEventCommand; parent: TEBObject): TEBObject;
begin
   result := TEBFlashObj.Create(parent);
   result.add(EventDeref(opcode.data[0]));
   result.Values.AddRange([opcode.Data[1], opcode.Data[2], opcode.Data[3],
                           opcode.Data[4], opcode.Data[5], opcode.Data[6]]);
end;

function ConvertMove(opcode: TEventCommand; parent: TEBObject): TEBObject;
var
   i: integer;
   moveString: AnsiString;
   route: TMoveOrder;
begin
   result := TEBMoveMapObj.Create(parent);
   result.add(EventDeref(opcode.data[0]));
   result.Values.AddRange([opcode.Data[1], opcode.Data[2], opcode.Data[3]]);
   setLength(moveString, length(opcode.data) - 4);
   for i := 4 to high(opcode.data) do
      moveString[i - 3] := AnsiChar(byte(opcode.data[i]));
   route := TMoveOrder.Create(moveString, false);
   try
      result.text := TPath.CalculateMoveBaseString(route);
   finally
      route.free;
   end;
end;

function ConvertEncounterRate(opcode: TEventCommand; parent: TEBObject): TEBObject;
begin
   result := TEBEncounterRate.Create(parent);
   result.Values.AddRange([1, opcode.Data[0]]);
end;

function ConvertWhileLoop(opcode: TEventCommand; parent: TEBObject): TEBObject;
begin
   result := TEBWhileLoop.Create(parent);
   inc(LoopDepth);
end;

function LoopEnd(opcode: TEventCommand; parent: TEBObject): TEBObject;
begin
   result := nil;
   dec(LoopDepth);
end;

function ConvertBreak(opcode: TEventCommand; parent: TEBObject): TEBObject;
begin
   if LoopDepth > 0 then
      result := TEBBreak.Create(parent)
   else result := TEBExit.Create(parent);
end;

function ConvertDelete(opcode: TEventCommand; parent: TEBObject): TEBObject;
begin
   result := TEBDeleteObj.Create(parent);
   result.Values.Add(0);
end;

function ConvertClassChange(opcode: TEventCommand; parent: TEBObject): TEBObject;
begin
   result := ObjectFactory(TEBClassChange, opcode, parent);
   assert(opcode.Data[0] = 1);
   result.Values.Delete(0);
end;

function ConvertBattleCommand(opcode: TEventCommand; parent: TEBObject): TEBObject;
begin
   result := ObjectFactory(TEBBattleCommand, opcode, parent);
   assert(opcode.Data[0] = 1);
   result.Values.Delete(0);
   result.values[2] := result.Values[2] xor 1;
end;

function ConvertMonsterHP(opcode: TEventCommand; parent: TEBObject): TEBObject;
begin
   result := ObjectFactory(TEBMonsterHP, opcode, parent);
   result.Values[0] := result.Values[0] + 1;
end;

function ConvertMonsterMP(opcode: TEventCommand; parent: TEBObject): TEBObject;
begin
   result := ObjectFactory(TEBMonsterMP, opcode, parent);
   result.Values[0] := result.Values[0] + 1;
end;

function ConvertMonsterStatus(opcode: TEventCommand; parent: TEBObject): TEBObject;
begin
   result := ObjectFactory(TEBMonsterStatus, opcode, parent);
   result.Values[0] := result.Values[0] + 1;
end;

function ConvertShowMonster(opcode: TEventCommand; parent: TEBObject): TEBObject;
begin
   result := ObjectFactory(TEBShowMonster, opcode, parent);
   result.Values[0] := result.Values[0] + 1;
end;

function ConvertBattleGlobalEvent(opcode: TEventCommand; parent: TEBObject): TEBObject;
begin
   result := TEBCallEvent.Create(parent);
   result.Values.AddRange([0, opcode.Data[0]]);
end;

function ConvertSysBGM(opcode: TEventCommand; parent: TEBObject): TEBObject;
begin
   case opcode.data[0] of
      0..2: result := ObjectFactory(TEBSysBGM, opcode, parent);
      3..5: begin
         opcode.data[0] := opcode.data[0] - 2;
         result := ObjectFactory(TEBVehicleBGM, opcode, parent);
      end;
      6: begin
         opcode.data[0] := 3;
         result := ObjectFactory(TEBSysBGM, opcode, parent);
      end;
      else raise EParseMessage.CreateFmt('Unknown BGM index %d', [opcode.data[0]]);
   end;
end;

function ConvertEraseScreen(opcode: TEventCommand; parent: TEBObject): TEBObject;
begin
   inc(opcode.data[0]);
   result := ObjectFactory(TEBEraseScreen, opcode, parent);
end;

function ConvertShowScreen(opcode: TEventCommand; parent: TEBObject): TEBObject;
begin
   inc(opcode.data[0]);
   result := ObjectFactory(TEBShowScreen, opcode, parent);
end;

function ConvertTransition(opcode: TEventCommand; parent: TEBObject): TEBObject;
begin
   inc(opcode.data[1]);
   result := ObjectFactory(TEBTransition, opcode, parent);
end;

function ConvertInputHeroName(opcode: TEventCommand; parent: TEBObject): TEBObject;
begin
   result := TEBInputHeroName.Create(parent);
   result.Values.Add(opcode.data[0]);
   result.Values.Add(opcode.data[2]);
end;

function ConvertTeleport(opcode: TEventCommand; parent: TEBObject): TEBObject;
begin
   result := ObjectFactory(TEBTeleport, opcode, parent);
   if result.Values.Count = 3 then
      result.Values.Add(0);
end;

function ConvertCallEvent(opcode: TEventCommand; parent: TEBObject): TEBObject;
begin
   result := ObjectFactory(TEBCallEvent, opcode, parent);
   if (result.values[0] = 1) and (result.values[1] = 10005) then
      result.values[1] := 0;
end;

function ConvertFlashScreen(opcode: TEventCommand; parent: TEBObject): TEBObject;
begin
   if length(opcode.data) <> 7 then
   begin
      assert(length(opcode.data) = 6);
      result := ObjectFactory(TEBFlashScreen, opcode, parent);
      result.Values.Add(0);
   end
   else begin
      if opcode.data[6] = 2 then
         result := TEBEndFlash.Create(parent)
      else result := ObjectFactory(TEBFlashScreen, opcode, parent);
   end;
end;

function ConvertShakeScreen(opcode: TEventCommand; parent: TEBObject): TEBObject;
begin
   if length(opcode.data) <> 5 then
   begin
      assert(length(opcode.data) = 4);
      result := ObjectFactory(TEBShakeScreen, opcode, parent);
      result.Values.Add(0);
   end
   else begin
      if opcode.data[4] = 2 then
         result := TEBEndShake.Create(parent)
      else result := ObjectFactory(TEBShakeScreen, opcode, parent);
   end;
end;

const
   OPCODES_IMPLEMENTED = 59;
   OPCODE_LIST: array[1..OPCODES_IMPLEMENTED] of TOpcodePair =
     ((K: 10; V: nil), (K: 20713; v: nil), (K: 20720; V: nil), (K: 20730; V: nil),
      (K: 10110; V: TEBShowMessage), (K: 20110; V: TEBExtension), (K: 10130; V: TEBPortrait),
      (K: 11410; V: TEBWait), (K: 10150; V: TEBInputNumber), (K: 10230; V: TEBTimer),
      (K: 10310; V: TEBMoney), (K: 10410; V: TEBExperience), (K: 10420; V: TEBLevel),
      (K: 10500; V: TEBTakeDamage), (K: 10610; V: TEBHeroName), (K: 10620; V: TEBHeroTitle),
      (K: 10670; V: TEBSysSFX), (K: 10680; V: TEBSysSkin),
      (K: 10820; V: TEBMemorizeLocation), (K: 10830; V: TEBMemoTeleport), (K: 10840; V: TEBRideVehicle),
      (K: 10910; V: TEBTerrainID), (K: 10920; V: TEBMapObjID), (K: 11030; V: TEBTintScreen),
      (K: 11060; V: TEBPanScreen), (K: 11070; V: TEBWeather),
      (K: 11340; V: TEBWaitMove), (K: 11350; V: TEBStopMove), (K: 11510; V: TEBPlayBGM),
      (K: 11520; V: TEBFadeBGM), (K: 11530; V: TEBMemBGM), (K: 11540; V: TEBPlayMemBGM),
      (K: 11550; V: TEBPlaySFX), (K: 11560; V: TEBPlayMovie), (K: 11610; V: TEBInput),
      (K: 11710; V: TEBChangeTileset), (K: 11720; V: TEBChangeBG), (K: 11130; V: TEBImageErase),
      (K: 11750; V: TEBTileSub), (K: 11810; V: TEBTeleLoc), (K: 11820; V: TEBTeleEnable),
      (K: 11830; V: TEBEscapeLoc), (K: 11840; V: TEBEscapeEnable), (K: 11910; V: TEBSave),
      (K: 11930; V: TEBSaveEnable), (K: 11950; V: TEBMenu), (K: 11960; V: TEBMenuEnable),
      (K: 12110; V: TEBLabel), (K: 12120; V: TEBGoto), (K: 12310; V: TEBExit),
      (K: 12410; V: TEBComment), (K: 22410; V: TEBExtension),
      (K: 12420; V: TEBGameOver), (K: 12510; V: TEBTitleScreen), (K: 13210; V: TEBBattleBG),
      (K: 13410; V: TEBEndBattle), (K: 1006; V: TEBForceFlee), (K: 1007; V: TEBEnableCombo),
      (K: 13260; V: TEBBattleAnimation));

   COMPLEX_OPCODES = 65;
   COMPLEX: array[1..COMPLEX_OPCODES] of TComplexOpcodePair =
     ((K: 0; V: Cleanup), (K: 10120; v: ConvertMessageOptions), (K: 20140; v: ConvertCaseExtension),
      (K: 10140; V: ConvertCase), (K: 20141; V: ConvertEndCase), (K: 12010; V: ConvertIf),
      (K: 22010; V: ConvertIfElse), (K: 22011; V: ConvertEndIf), (K: 10710; V: ConvertBattle),
      (K: 20710; v: ConvertVictory), (K: 20711; V: ConvertEscape), (K: 20712; V: ConvertDefeat),
      (K: 10210; V: ConvertSwitch), (K: 10220; V: ConvertVar), (K: 10320; V: ConvertInventory),
      (K: 10330; V: ConvertParty), (K: 10430; V: ConvertStats), (K: 10440; V: ConvertSkills),
      (K: 10450; V: ConvertEquipment), (K: 10460; V: ConvertHP), (K: 10470; V: ConvertMP),
      (K: 10480; V: ConvertStatus), (K: 10490; V: ConvertFullHeal), (K: 10630; V: ConvertSetSprite),
      (K: 10640; V: ConvertSetPortrait), (K: 10650; V: ConvertVSprite), (K: 10720; V: ConvertShop),
      (K: 20721; V: ConvertIfElse), (K: 20722; V: ConvertEndIf), (K: 10730; V: ConvertInn),
      (K: 20731; V: ConvertIfElse), (K: 20732; V: ConvertEndIf), (K: 10850; V: ConvertTeleportVehicle),
      (K: 10860; V: ConvertTeleportEvent), (K: 10870; V: ConvertSwapObjects), (K: 11040; V: ConvertFlashScreen),
      (K: 11110; V: ConvertNewImage), (K: 11120; V: ConvertMoveImage), (K: 11210; V: ConvertShowAnim),
      (K: 11310; V: ConvertTranslucency), (K: 11320; V: ConvertFlash), (K: 11330; V: ConvertMove),
      (K: 11740; V: ConvertEncounterRate), (K: 12210; V: ConvertWhileLoop), (K: 22210; V: LoopEnd),
      (K: 12220; V: ConvertBreak), (K: 12320; V: ConvertDelete), (K: 1008; V: ConvertClassChange),
      (K: 1009; V: ConvertBattleCommand), (K: 10660; V: ConvertSysBGM), (K: 11050; V: ConvertShakeScreen),
      (K: 13110; V: ConvertMonsterHP), (K: 13120; V: ConvertMonsterMP), (K: 13130; V: ConvertMonsterStatus),
      (K: 13150; V: ConvertShowMonster), (K: 1005; V: ConvertBattleGlobalEvent),
      (K: 13310; V: ConvertBattleIf), (K: 23310; V: ConvertIfElse), (K: 23311; V: ConvertEndIf),
      (K: 11010; V: ConvertEraseScreen), (K: 11020; V: ConvertShowScreen),
      (K: 10690; V: ConvertTransition), (K: 10740; V: ConvertInputHeroName),
      (K: 10810; V: ConvertTeleport), (K: 12330; V: ConvertCallEvent));

var
   i: integer;

initialization
   dict := TOpcodeDictionary.Create;
   for I := low(OPCODE_LIST) to high(OPCODE_LIST) do
      dict.Add(OPCODE_LIST[i].K, OPCODE_LIST[i].V);
   cDict := TComplexOpcodeDictionary.Create;
   for I := low(COMPLEX) to high(COMPLEX) do
      cDict.Add(COMPLEX[i].K, COMPLEX[i].V);
   blockStack := TStack<integer>.Create;
   ifStack := TStack<TEBIf>.Create;
   tally := TUnknownOpcodeDictionary.Create;
finalization
   ifStack.Free;
   blockStack.Free;
   dict.Free;
   cDict.Free;
   tally.Free;
end.
