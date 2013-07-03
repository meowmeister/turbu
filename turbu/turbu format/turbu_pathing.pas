unit turbu_pathing;

interface
uses
   Generics.Collections, Classes,
   turbu_defs,
   dwsJSON;

type
   TMoveStep = record
      opcode: byte;
      name: string;
      data: array[1..3] of word;
      constructor Create(opcode: byte); overload;
      constructor Create(opcode: byte; value: word); overload;
   end;

   TMoveList = class(TList<TMoveStep>);

   TPath = class(TObject)
   private
      FOpcodes: TMoveList;
      FBase: string;
      FCursor: integer;
      FIterationCursor: integer;
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
      function Clone: TPath;
      procedure Serialize(writer: TdwsJSONWriter);
      constructor Deserialize(obj: TdwsJSONObject);

      function nextCommand: TMoveStep;
      procedure setDirection(direction: TFacing);

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
   rsLexer, rsDefs;

var
   moveDic: TDictionary<string,integer>;

function parseStep(lexer: TrsLexer): TMoveStep; forward;

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
   FillChar(step.data, length(step.data), 0);
   FOpcodes.Add(step);
end;

function TPath.Clone: TPath;
begin
   result := TPath.Assign(self);
   result.FCursor := self.FCursor;
   result.FIterationCursor := self.FIterationCursor;
   result.FLooped := self.FLooped;
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
   lexer: TrsLexer;
begin
   FOpcodes := TMoveList.Create;
   FLoop := loop;
   FBase := data;
   if data = '' then
      Exit;

   lexer := TrsLexer.Create;
   try
      if data[length(data)] <> ';' then
         data := data + ';';
      lexer.Load(data);
      while not lexer.eof do
         FOpcodes.Add(parseStep(lexer))
   finally
      lexer.Free;
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
   inc(FIterationCursor);
   if (result.opcode in CODES_WITH_PARAMS) or (FIterationCursor >= result.data[1]) then
   begin
      inc(FCursor);
      FIterationCursor := 0;
   end;
end;

procedure TPath.Save(savefile: TStream);
begin
   savefile.writeString(FBase);
   savefile.writeBool(FLoop);
end;

procedure TPath.Serialize(writer: TdwsJSONWriter);
begin
   writer.BeginObject;
      writer.CheckWrite('Path', FBase, '');
      writer.CheckWrite('Cursor', FCursor, 0);
      writer.CheckWrite('Looped', FLooped, false);
   writer.EndObject;
end;

constructor TPath.Deserialize(obj: TdwsJSONObject);
begin
   obj.CheckRead('Path', FBase);
   obj.CheckRead('Cursor', FCursor);
   obj.CheckRead('Looped', FLooped);
   obj.CheckEmpty;
end;

procedure TPath.setDirection(direction: TFacing);
var
   newStep: TMoveStep;
begin
   FLoop := false;
   FOpcodes.Clear;
   newStep.opcode := Ord(direction);
   FillChar(newStep.data, length(newStep.data), 0);
   FOpcodes.Add(newStep);
   FCursor := 0;
end;

function lookupMoveCode(const opcode: string): integer;
begin
   if not moveDic.TryGetValue(UpperCase(opcode), result) then
      result := -1;
end;

procedure parseAssert(lexer: TrsLexer; kind: TTokenKind);
var
   token: TToken;
begin
   lexer.Lex(token);
   assert(token.kind = kind);
end;

function parseInt(lexer: TrsLexer): integer;
var
   token: TToken;
begin
   lexer.Lex(token);
   assert(token.kind = tkInt);
   result := strToInt(token.origText);
end;

procedure parseName(lexer: TrsLexer; var step: TMoveStep);
var
   token: TToken;
begin
   lexer.Lex(token);
   assert(token.kind = tkString);
   step.name := AnsiDequotedStr(token.origText, '''');
end;

procedure parseOneInt(lexer: TrsLexer; var step: TMoveStep);
begin
   step.data[1] := parseInt(lexer);
end;

procedure parseNameAndInt(lexer: TrsLexer; var step: TMoveStep);
begin
   parseName(lexer, step);
   parseAssert(lexer, tkComma);
   parseOneInt(lexer, step);
end;

procedure parseFull(lexer: TrsLexer; var step: TMoveStep);
var
   i: integer;
begin
   parseName(lexer, step);
   parseAssert(lexer, tkComma);
   for I := 1 to 3 do
   begin
      step.data[i] := parseInt(lexer);
      if i <> 3 then
         parseAssert(lexer, tkComma);
   end;
end;

function parseStep(lexer: TrsLexer): TMoveStep;
var
   token: TToken;
begin
   lexer.Lex(token);
   assert(token.kind = tkIdentifier);
   result.opcode := lookupMoveCode(token.origText);
   FillChar(result.data, length(result.data), 0);
   lexer.Lex(token);
   if (result.opcode in CODES_WITH_PARAMS) then
      assert(token.kind = tkOpenParen);
   if token.kind = tkOpenParen then
   begin
      case result.opcode of
         $22: parseNameAndInt(lexer, result);
         $23: parseFull(lexer, result);
         else parseOneInt(lexer, result);
      end;
      parseAssert(lexer, tkCloseParen);
      parseAssert(lexer, tkSem);
   end
   else assert(token.kind = tkSem);
end;

{ TMoveStep }

constructor TMoveStep.Create(opcode: byte);
begin
   self.opcode := opcode;
   data[1] := 0;
   data[2] := 0;
   data[3] := 0;
end;

constructor TMoveStep.Create(opcode: byte; value: word);
begin
   self.opcode := opcode;
   data[1] := value;
   data[2] := 0;
   data[3] := 0;
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
