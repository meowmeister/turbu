unit rm2_turbu_event_builder;

interface
uses
   Classes,
   Events, EventBuilder;

function ConvertOpcode(opcode: TEventCommand; parent: TEBObject): TEBObject;
function UnknownOpcodeList: TStringList;

implementation
uses
windows,
   SysUtils, Generics.Defaults, Generics.Collections,
   turbu_defs, commons,
   EB_RpgScript, EB_System, EB_Messages, EB_Expressions;

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

var
   dict: TOpcodeDictionary;
   cDict: TComplexOpcodeDictionary;
   blockStack: TStack<integer>;
   ifStack: TStack<TEBIf>;
   tally: TUnknownOpcodeDictionary;

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
   result.Text := string(opcode.name);
   result.Values.AddRange(opcode.data);
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
   result := TEBChoiceMessage.Create(parent, string(opcode.name), opcode.Data[0]);
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
      assert(blockStack.Pop >= 0);
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
      2: left := TEBPropExpr.Create('level');
      3: left := TEBPropExpr.Create('HP');
      4: left := TEBLookupValue.Create(d3, 'skills');
      5:
      begin
         left := TEBCall.Create('Equipped');
         left.Add(TEBLookupValue.Create(d3, 'items'));
         TEBCall(left).hint := 3;
      end;
      6: left := TEBLookupValue.Create(d3, 'conditions');
   end;

   if d2 <> 1 then
      right := nil;
   if d2 in [2, 3] then
      op := co_gtE
   else op := co_equals;
end;

function eventDeref(const data: integer): TEBObjExpr;
begin
   case data of
      10001: result := TEBObjExpr.Create('Party');
      10002: result := TEBObjExpr.Create('Boat');
      10003: result := TEBObjExpr.Create('Ship');
      10004: result := TEBObjExpr.Create('Airship');
      10005: result := TEBObjExpr.Create('ThisEvent');
      else result := TEBObjArrayValue.Create('Event', data);
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
         case boolean(opcode.data[2]) of
            false: right := TEBIntegerValue.Create(opcode.data[3]);
            true: right := TEBIntsValue.Create(opcode.data[3]);
         end;
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
         left := TEBLookupObjExpr.Create('hero', opcode.Data[1], 'heroes', TEBChainable(left));
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
      9:
      begin
         left := TEBPropExpr.Create('looped');
         left := TEBObjExpr.Create('bgm', TEBPropExpr(left));
      end;
   end;
   if right = nil then
      right := TEBBooleanValue.Create(true);
   result := TEBIf.Create(parent, left, right, op);
   blockStack.push(-1);
   ifStack.Push(TEBIf(result));
end;

function ConvertIfElse(opcode: TEventCommand; parent: TEBObject): TEBObject;
begin
   assert(blockStack.peek = -1);
   ifStack.Peek.SetElse;
   result := objectFactory(TEBElseBlock, opcode, parent);
end;

function ConvertEndIf(opcode: TEventCommand; parent: TEBObject): TEBObject;
begin
   assert(blockStack.pop = -1);
   ifStack.pop;
   result := nil;
end;

function ConvertBattle(opcode: TEventCommand; parent: TEBObject): TEBObject;
var
   call: TEBCall;
   block: TEBEnumCaseBlock;
begin
   call := TEBCall.Create('battle');
   if boolean(opcode.data[0]) then
      call.add(TEBIntsValue.Create(opcode.Data[1]))
   else call.add(TEBLookupValue.Create(opcode.data[1], 'mparties'));
   call.add(TEBStringValue.Create(string(opcode.Name)));
   call.add(TEBBooleanValue.Create(boolean(opcode.data[3])));
   call.add(TEBBooleanValue.Create(boolean(opcode.data[5])));
   if (opcode.Data[3] = 0) and (opcode.Data[4] = 0) then
      result := TEBFunctionCall.Create(parent, call)
   else if (boolean(opcode.Data[4]) = true) or (opcode.Data[3] = 2) then
   begin
      result := TEBCase.Create(parent, call);
      if opcode.data[3] = 1 then
      begin
         block := TEBEnumCaseBlock.Create(result);
         block.Text := 'br_escaped';
         block.Add(TEBExit.Create(nil));
      end;
   end
   else if opcode.data[3] = 1 then
   begin
      result := TEBIf.Create(parent, call, TEBEnumValue.Create('br_escaped'), co_equals);
      result.add(TEBExit.Create(nil));
   end
   else begin
      call.Free;
      raise ERPGScriptError.Create('Unknown battle opcode configuration!');
   end;
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
   result.text := 'br_defeat';
end;

function CreateForLoop(parent, child: TEBObject; lbound, ubound: integer): TEBForLoop;
begin
   result := TEBForLoop.Create(parent);
   result.add(child);
   result.Values.add(lbound);
   result.Values.add(ubound);
   result.text := 'Num';
end;

function CreateSubscript(mode, data: integer): TEBExpression;
begin
   case mode of
      0: result := TEBIntegerValue.Create(data);
      1: result := TEBVariableValue.Create('Num');
      2: result := TEBIntsValue.Create(data);
      else raise ERPGScriptError.CreateFmt('Unknown switch data 0 value: %d!', [mode]);
   end;
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
   EVENTPROPS: array[0..5] of string = ('MapID', 'xPos', 'yPos', 'facing', 'ScreenX', 'ScreenY');
   MISCPROPS: array[0..9] of string = ('money', 'timer.time', 'partySize', 'saveCount',
                                       'battles', 'victories', 'losses', 'flees',
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
      else raise ERPGScriptError.CreateFmt('Unknown variable data 4 value: %d!', [opcode.Data[4]]);
   end;
   if opcode.data[3] > 0 then
      rValue := TEBBinaryOp.Create(CreateSubscript(opcode.data[0], opcode.data[1]),
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
      expr := TEBLookupValue.Create(opcode.Data[2], 'Items')
   else expr := TEBIntsValue.Create(opcode.Data[2]);
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

const
   OPCODES_IMPLEMENTED = 10;
   OPCODE_LIST: array[1..OPCODES_IMPLEMENTED] of TOpcodePair =
     ((K: 0; V: nil), (K: 10; V: nil), (K: 20713; v: nil), (K: 10110; V: TEBShowMessage),
      (K: 20110; V: TEBExtension), (K: 10130; V: TEBPortrait), (K: 11410; V: TEBWait),
      (K: 10150; V: TEBInputNumber), (K: 10230; V: TEBTimer), (K: 10310; V: TEBMoney));

   COMPLEX_OPCODES = 15;
   COMPLEX: array[1..COMPLEX_OPCODES] of TComplexOpcodePair =
     ((K: 10120; v: ConvertMessageOptions), (K: 20140; v: ConvertCaseExtension),
      (K: 10140; v: ConvertCase), (K: 20141; v: ConvertEndCase), (K: 12010; v: ConvertIf),
      (K: 22010; v: ConvertIfElse), (K: 22011; v: ConvertEndIf), (K: 10710; v: ConvertBattle),
      (K: 20710; v: ConvertVictory), (K: 20711; v: ConvertEscape), (K: 20712; v: ConvertDefeat),
      (K: 10210; v: ConvertSwitch), (K: 10220; v: ConvertVar), (K: 10320; V: ConvertInventory),
      (K: 10330; v: ConvertParty));

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
