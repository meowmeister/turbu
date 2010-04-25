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
   SysUtils,
   turbu_defs, rm2_turbu_maps;

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
begin
   FId := id;
   self.FConditions := TRpgEventConditions.Convert(base.conditionBlock);
   if base.filename = '' then
      CalculateTileID(base.whichChip)
   else FName := format('%s %d', [string(base.filename), base.whichChip]);
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
