unit rm2_turbu_map_objects;

interface
uses
   events, turbu_map_objects, move_data, turbu_pathing;

type
   T2k2RpgMapObject = class helper for TRpgMapObject
   public
      constructor Convert(base: TEvent);
   end;

   T2k2RpgEventPage = class helper for TRpgEventPage
   private
      procedure CalculateTileID(tile: word);
      procedure CreateEventScript(base: TEventPage);
   public
      constructor Convert(base: TEventPage; id: integer; parent: TRpgMapObject);
   end;

   T2k2RpgEventConditions = class helper for TRpgEventConditions
   private
      function convertConditionals(base: events.TPageConditions): TPageConditionSet;
   public
      constructor Convert(base: TEventConditions);
   end;

   T2k2Path = class helper for TPath
   private
      function CalculateParamList(const rec: TMoveRecord): string;
      function CalculateMoveCommand(const rec: TMoveRecord): string;
      function CalculateMoveBaseString(base: TMoveOrder): string;
   public
      constructor Convert(base: TEventMoveBlock);
   end;

implementation
uses
   SysUtils, Generics.Collections, Classes,
   charset_data, turbu_defs, rm2_turbu_maps, rm2_turbu_event_builder,
   EventBuilder, EB_RpgScript;

{ T2k2RpgMapObject }

constructor T2k2RpgMapObject.Convert(base: TEvent);
var
  I: Integer;
begin
   inherited Create;
   FId := base.id;
   FName := string(base.name);
   location := base.location;
   for I := 1 to base.len do
      AddPage(TRpgEventPage.Convert(base.page[i - 1], i - 1, self));
end;

function T2k2Path.CalculateMoveBaseString(base: TMoveOrder): string;
const
   SEM = '; ';
var
   i: Integer;
begin
   result := '';
   for i := 0 to base.last do
      result := result + CalculateMoveCommand(base.command[i]) + SEM;
   if result <> '' then
      delete(result, length(result), 1);
end;

function T2k2Path.CalculateMoveCommand(const rec: TMoveRecord): string;
const
   PARAM_CODE = '%s(%s)';
begin
   result := MOVE_CODES[rec.opcode];
   if rec.opcode in CODES_WITH_PARAMS then
      result := format(PARAM_CODE, [result, CalculateParamList(rec)]);
end;

function T2k2Path.CalculateParamList(const rec: TMoveRecord): string;
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

{ T2k2RpgEventPage }

procedure T2k2RpgEventPage.CalculateTileID(tile: word);
var
   dec: TDecodeResult;
begin
   dec := decode(tile + 10000);
   self.whichTile := dec.baseTile;
   FName := '*' + intToStr(dec.group + 9); //* character can't be used in filenames
end;

constructor T2k2RpgEventPage.Convert(base: TEventPage; id: integer; parent: TRpgMapObject);
const
   FRAMES: array[TFacing] of word = (1, 4, 7, 10);
begin
   FId := id;
   self.FConditions := TRpgEventConditions.Convert(base.conditionBlock);
   if base.filename = '' then
      CalculateTileID(base.whichChip)
   else begin
      FName := format('%s %d', [string(base.filename), base.whichChip]);
      self.whichTile := FRAMES[base.direction];
   end;
   self.direction := base.direction;
   self.transparent := base.transparent;
   self.path := TPath.Convert(base.moveBlock);
   self.moveType := TMoveType(base.moveType);
   self.moveFrequency := base.moveFrequency;
   self.startCondition := TStartCondition(base.startCondition);
   self.zOrder := base.zOrder;
   self.isBarrier := base.is_barrier;
   self.animType := TAnimType(base.animType);
   self.moveSpeed := base.moveSpeed;
   self.FParent := parent;
   if base.opcode.count > 0 then
      CreateEventScript(base);
end;

procedure T2k2RpgEventPage.CreateEventScript(base: TEventPage);
const
   PROCNAME = '%s_page%d';
var
   proc: TEBProcedure;
   command: TEventCommand;
   last: TEBObject;
   stack: TStack<TEBObject>;
   fudgeFactor: integer;
begin
   fudgeFactor := 0;
   stack := TStack<TEBObject>.Create;
   proc := TEBProcedure.Create(nil);
   proc.name := format(PROCNAME, [self.parent.name, self.id]);
   try
   try
      last := proc;
      for command in base.opcode do
      begin
         if (command.opcode = 20141) or (command.opcode = 20713) then
            dec(fudgeFactor);
         if command.indent + fudgeFactor >= stack.Count then
            stack.Push(last)
         else if command.indent + fudgeFactor < stack.Count - 1 then
            stack.Pop;
         if (command.opcode = 20110) then
            ConvertOpcode(command, last)
         else last := ConvertOpcode(command, stack.Peek);
         if (command.opcode = 10140) or
            ((command.opcode = 10710) and ((command.data[3] <> 0) or (command.data[4] <> 0))) then
            inc(fudgeFactor);
      end;
      assert(stack.Count = 1);
      assert(fudgeFactor = 0);
      self.FEventText := proc.serialize;
   except
      proc.SaveScript;
      raise;
   end;
   finally
      stack.Free;
      proc.Free;
   end;
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

end.
