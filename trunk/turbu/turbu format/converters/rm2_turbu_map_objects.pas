unit rm2_turbu_map_objects;

interface
uses
   events, turbu_map_objects, move_data;
type
   T2k2RpgMapObject = class helper for TRpgMapObject
   public
      constructor Convert(base: TEvent);
   end;

   T2k2RpgEventPage = class helper for TRpgEventPage
   private
      function CalculateParamList(const rec: TMoveRecord): string;
      function CalculateMoveCommand(const rec: TMoveRecord): string;
      function CalculateMoveBaseString(base: TMoveOrder): string;
      procedure CalculateTileID(tile: word);
   public
      constructor Convert(base: TEventPage; id: integer; parent: TRpgMapObject);
   end;

   T2k2RpgEventConditions = class helper for TRpgEventConditions
   public
      constructor Convert(base: TEventConditions);
   end;

implementation
uses
   SysUtils,
   turbu_pathing,
   rm2_turbu_maps;

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

{ T2k2RpgEventPage }

function T2k2RpgEventPage.CalculateMoveBaseString(base: TMoveOrder): string;
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

function T2k2RpgEventPage.CalculateMoveCommand(const rec: TMoveRecord): string;
const
   PARAM_CODE = '%s(%s)';
begin
   result := MOVE_CODES[rec.opcode];
   if rec.opcode in CODES_WITH_PARAMS then
      result := format(PARAM_CODE, [result, CalculateParamList(rec)]);
end;

function T2k2RpgEventPage.CalculateParamList(const rec: TMoveRecord): string;
const
   ADD_INT = '%s, %d';
var
   i: integer;
begin
   case rec.opcode of
      $20, $21: result := intToStr(rec.data[1]);
      $22: result := QuotedStr(rec.name);
      $23:
      begin
         result := QuotedStr(rec.name);
         for i := 1 to 3 do
            result := format(ADD_INT, [result, rec.data[i]]);
      end;
   end;
end;

procedure T2k2RpgEventPage.CalculateTileID(tile: word);
var
   dec: TDecodeResult;
begin
   dec := decode(tile + 10000);
   self.whichTile := dec.baseTile;
   FName := '*' + intToStr(dec.group); //* character can't be used in filenames
end;

constructor T2k2RpgEventPage.Convert(base: TEventPage; id: integer; parent: TRpgMapObject);
var
   movebase: string;
begin
   FId := id;
   FName := string(base.filename);
   self.FConditions := TRpgEventConditions.Convert(base.conditionBlock);
   if base.filename = '' then
      CalculateTileID(base.whichChip)
   else self.whichTile := base.whichChip;
   self.direction := base.direction;
   self.transparent := base.transparent;
   if assigned(base.moveBlock.moveBlock) then
   begin
      movebase := CalculateMoveBaseString(base.moveBlock.moveBlock);
      self.path := TPath.Create(movebase, base.moveBlock.moveBlock.loop);
   end
   else self.path := TPath.Create;
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
   self.switch1Set := base.switch1Set;
   self.switch2Set := base.switch2Set;
   self.variable1Set := base.variableSet;
   self.variable1Value := base.variableValue;
   self.itemNeeded := base.itemNeeded;
   self.heroNeeded := base.heroNeeded;
   self.timeRemaining := base.timeRemaining;
end;

end.
