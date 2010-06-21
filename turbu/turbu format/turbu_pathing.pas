unit turbu_pathing;

interface
uses
   Generics.Collections, Classes,
   turbu_defs;

type
   TMoveStep = record
      opcode: byte;
      name: string;
      data: array[1..3] of word;
   end;

   TMoveList = class(TList<TMoveStep>);

   TPath = class(TObject)
   private
      FOpcodes: TMoveList;
      FBase: string;
      FCursor: integer;
      FLoop: boolean;
      FLooped: boolean;
    function GetLast: integer;

   public
      constructor Create; overload;
      constructor Create(data: string; loop: boolean); overload;
      constructor Create(direction: TFacing); overload;
      constructor Assign(copy: TPath);
      constructor Load(savefile: TStream);
      procedure Save(savefile: TStream);
      destructor Destroy; override;

      function nextCommand: TMoveStep;

      property last: integer read GetLast;
      property base: string read FBase;
      property opcodes: TMovelist read FOpcodes;
      property cursor: integer read FCursor write FCursor;
      property loop: boolean read FLoop;
      property looped: boolean read FLooped write FLooped;
   end;

//   TRouteSet = array of TMoveOrder;

const MOVE_CODES: array[0..$29] of string =
('Up', 'Right', 'Down', 'Left', 'UpRight', 'DownRight', 'DownLeft', 'UpLeft',
'RandomStep', 'TowardsHero', 'AwayFromHero', 'MoveForward', 'FaceUp', 'FaceRight',
'FaceDown', 'FaceLeft', 'TurnRight', 'TurnLeft', 'Turn180', 'Turn90',
'FaceRandom', 'FaceHero', 'FaceAwayFromHero', 'Pause', 'StartJump', 'EndJump',
'FacingFixed', 'FacingFree', 'SpeedUp', 'SpeedDown', 'FreqUp', 'FreqDown',
'SwitchOn', 'SwitchOff', 'ChangeSprite', 'PlaySfx', 'ClipOff', 'ClipOn',
'AnimStop', 'AnimResume', 'TransparencyUp', 'TransparencyDown');

CODES_WITH_PARAMS = [$20..$23];

const
   MOVECODE_RANDOM = 8;
   MOVECODE_CHASE = 9;
   MOVECODE_FLEE = 10;
   MOVECODE_CHANGE_SPRITE = $22;
   MOVECODE_PLAY_SFX = $23;

function lookupMoveCode(const opcode: string): integer;

implementation
uses
   SysUtils,
   turbu_classes,
   uPSUtils;

var
   moveDic: TDictionary<string,integer>;

function parseStep(parser: TPsPascalParser): TMoveStep; forward;

{ TPath }

constructor TPath.Assign(copy: TPath);
begin
   FBase := copy.base;
   FOpcodes := TMoveList.Create(copy.opcodes);
   FLoop := copy.loop;
end;

constructor TPath.Create(direction: TFacing);
var
   step: TMoveStep;
begin
   FOpcodes := TMoveList.Create;
   step.opcode := Ord(direction);
   FOpcodes.Add(step);
end;

destructor TPath.Destroy;
begin
   FOpcodes.Free;
   inherited;
end;

function TPath.GetLast: integer;
begin
   result := FOpcodes.Count - 1;
end;

constructor TPath.Create;
begin
   FOpcodes := TMoveList.Create;
end;

constructor TPath.Load(savefile: TStream);
var
   base: string;
   loop: boolean;
begin
   base := savefile.readString;
   loop := savefile.readBool;
   self.Create(base, loop);
end;

constructor TPath.Create(data: string; loop: boolean);
var
   parser: TPSPascalParser;
begin
   FOpcodes := TMoveList.Create;
   FLoop := loop;
   FBase := data;
   if data = '' then
      Exit;

   parser := TPSPascalParser.Create;
   try
      if data[length(data)] <> ';' then
         data := data + ';';
      parser.SetText(AnsiString(data));
      while parser.CurrTokenID <> CSTI_EOF do
         FOpcodes.Add(parseStep(parser))
   finally
      parser.Free;
   end;
end;

function TPath.nextCommand: TMoveStep;
begin
   if FCursor >= FOpcodes.Count then
      if loop then
      begin
         FCursor := 0;
         FLooped := true;
      end else begin
         result.opcode := $30;
         Exit;
      end;
   result := FOpcodes[FCursor];
   inc(FCursor);
end;

procedure TPath.Save(savefile: TStream);
begin
   savefile.writeString(FBase);
   savefile.writeBool(FLoop);
end;

function lookupMoveCode(const opcode: string): integer;
begin
   if not moveDic.TryGetValue(UpperCase(opcode), result) then
      result := -1;
end;

procedure parseAssert(parser: TPsPascalParser; token: TPsPasToken);
begin
   assert(parser.CurrTokenID = token);
   parser.next;
end;

function parseInt(parser: TPsPascalParser): integer;
begin
   assert(parser.CurrTokenID = CSTI_Integer);
   result := strToInt(parser.OriginalToken);
   parser.Next;
end;

procedure parseName(parser: TPsPascalParser; var step: TMoveStep);
begin
   assert(parser.CurrTokenID = CSTI_String);
   step.name := AnsiDequotedStr(string(parser.OriginalToken), '''');
   parser.Next;
end;

procedure parseOneInt(parser: TPsPascalParser; var step: TMoveStep);
begin
   step.data[1] := parseInt(parser);
end;

procedure parseNameAndInt(parser: TPsPascalParser; var step: TMoveStep);
begin
   parseName(parser, step);
   parseAssert(parser, CSTI_Comma);
   parseOneInt(parser, step);
end;

procedure parseFull(parser: TPsPascalParser; var step: TMoveStep);
var
   i: integer;
begin
   parseName(parser, step);
   parseAssert(parser, CSTI_Comma);
   for I := 1 to 3 do
   begin
      step.data[i] := parseInt(parser);
      if i <> 3 then
         parseAssert(parser, CSTI_Comma);
   end;
end;

function parseStep(parser: TPsPascalParser): TMoveStep;
begin
   assert(parser.CurrTokenID = CSTI_Identifier);
   result.opcode := lookupMoveCode(string(parser.GetToken));
   parser.Next;
   if (result.opcode in CODES_WITH_PARAMS) then
      assert(parser.CurrTokenID = CSTI_OpenRound);
   if parser.CurrTokenID = CSTI_OpenRound then
   begin
      parser.Next;
      if result.opcode in CODES_WITH_PARAMS then
         case result.opcode of
            $20, $21: parseOneInt(parser, result);
            $22: parseNameAndInt(parser, result);
            $23: parseFull(parser, result);
         end;
      parseAssert(parser, CSTI_CloseRound);
   end;
   parseAssert(parser, CSTI_SemiColon);
end;

var
   i: integer;
initialization
   moveDic := TDictionary<string, integer>.Create(high(MOVE_CODES) * 2);
   for I := low(MOVE_CODES) to high(MOVE_CODES) do
      moveDic.Add(UpperCase(MOVE_CODES[i]), i);

finalization
   moveDic.Free;

end.
