unit events;
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

{$D+} {$hints off}
interface

uses math, types, classes, Contnrs, turbu_containers, //system libs
     charset_data, LDB, LMT, move_data, //turbu libs
     uPSCompiler, upsUtils; //PascalScript lib

type
   TMoveType = (still, randomMove, cycle_ud, cycle_lr, chase_hero, flee_hero, by_route);
   TStartCondition = (by_key, by_touch, by_collision, automatic, parallel, on_call);
   TAnimType = (at_sentry, at_jogger, at_fixedDir, at_fixedJog, at_statue, at_spinRight);
   TPageConditionSet = (switch1, switch2, variable1, item, hero, timer, timer2);
   TCancelHandler = (cancelIgnore, cancel1, cancel2, cancel3, cancel4, cancelElse);
   TUsesList = (uses_messages, uses_party, uses_system, uses_sound, uses_graphics, uses_menu, uses_maps);

   TPageConditions = array[TPageConditionSet] of boolean;

type
   TCaseStackFrame = class(TObject)
   private
      FDepth: byte;
      FCount: byte;
      FElse: boolean;
      FCurrent: byte;
   public
      constructor Create(const depth, count: byte; const elseCase: boolean);

      property depth: byte read FDepth;
      property count: byte read FCount;
      property elseCase: boolean read FElse;
      property current: byte read FCurrent write FCurrent;
   end;

   TEventConditions = class(TObject)
   private
      FConditions: TPageConditions;
      FSwitch1: word;
      FSwitch2: word;
      FVariable: word;
      FVarValue: integer;
      FItem: word;
      FHero: byte;
      FClock: word;
      FClock2: word;
      FVarOperator: byte;
{$IFDEF ENGINE}
      function evaluate: boolean;
{$ENDIF}
   public
      constructor create(input: TStringStream);
      constructor createGlobal(input: TStream);

{$IFDEF ENGINE}
      property valid: boolean read evaluate;
{$ENDIF}
      property conditions: TPageConditions read FConditions write FConditions;
      property switch1Set: word read FSwitch1 write FSwitch1;
      property switch2Set: word read FSwitch2 write FSwitch2;
      property variableSet: word read FVariable write FVariable;
      property variableValue: integer read FVarValue write FVarValue;
      property itemNeeded: word read FItem write FItem;
      property heroNeeded: byte read FHero write FHero;
      property timeRemaining: word read fclock write FClock;
      property clock2: word read FClock2 write FClock2;
      property varOperator: byte read FVarOperator;
   end;
   TEvent = class;
   TEventBlock = class;
   TEventPage = class;

   TEventCommand = class(TObject)
   private
   class var
      FCorrection: byte;
   var
      FOpcode: integer;
      FDepth: byte;
      FName: ansiString;
      FData: TArray<integer>;
      FParent: TEventPage;
      function getNextCommand: TEventCommand;
      function getScript: ansiString;
      function eventDeref(const data: integer): ansiString;
      {$IFDEF EDITOR}
      function heroName(const id: word): ansiString;
      function itemName(const id: word): ansiString;
      function skillName(const id: word): ansiString;
      function conditionName(const id: word): ansiString;
      function mpartyName(const id: word): ansiString;
      function animName(const id: word): ansiString;
      function chipsetName(const id: word): ansiString;
      {$ENDIF}

      property next: TEventCommand read getNextCommand;
   public
      constructor Create(input: tstream; parent: TEventPage);
      property script: ansiString read getScript;
      property parent: TEventPage read FParent;
      property opcode: integer read FOpcode;
      property name: ansiString read FName;
      property data: TArray<integer> read FData;
      property indent: byte read FDepth;
   end;

   TEventMoveBlock = class(TObject)
   private
      FOrder: TMoveOrder;
      FIgnore: boolean;
   public
      constructor Create(input: TStream);
      destructor Destroy; override;

      property moveBlock: TMoveOrder read FOrder write FOrder;
      property ignore: boolean read FIgnore write FIgnore;
   end;

   TEventPage = class(TObject)
   private
      FID: word;
      FConditions: TEventConditions;
      FGraphicFile: ansiString;
      FGraphic: word;
      FTransparent: boolean;
      FOverrideFile: ansiString;
      FOverrideGraphic: word;
      FOverrideTransparency: boolean;
      FOverrideSprite: boolean;
      FDirection: TFacing;
      FFrame: TAnimFrame;
      FMoveType: TMoveType;
      FMoveFrequency: byte;
      FStartCondition: TStartCondition;
      FEventHeight: byte;
      FNoOverlap: boolean;
      FAnimType: TAnimType;
      FMoveSpeed: byte;
      FMoveScript: TEventMoveBlock;
      FEventScript: ansiString;
      FEventText: ansiString;
      FScriptText: ansiString;
      FCompiledScript: ansiString;
      FCommands: TRpgObjectList<TEventCommand>;
      FParent: TEvent;
      FParseStack: TStack;
      FLoopDepth: byte;
      FUseList: set of TUsesList;
      FIndexList: array[1..10] of boolean;
      FLabelList: ansiString;
      FOverride: boolean;
      FGlobal: boolean;
{$IFDEF ENGINE}
      function isValid: boolean; inline;
{$ENDIF}

      function getEventScript: ansiString;
      function getCompiledScript: tbtString;
      function hasScriptFunction: boolean; inline;

      procedure stripLastSem(var data: ansiString);
      procedure use(value: TUsesList);
      function getLength: word;
      function getGraphicFile: ansiString; inline;
      function getGraphic: word; inline;
      function getTransparent: boolean; inline;
   public
      constructor create(var input: TStringStream; const expected: word; parent: TEvent); overload;
      constructor create(input: ansiString; const expected: word; parent: TEvent); overload;
      constructor createGlobal(input: TStream; parent: TEvent);
      destructor Destroy; override;
      procedure overrideSprite(filename: ansiString; index: byte; transparent: boolean);

      property conditionBlock: TEventConditions read FConditions write FConditions;
      property filename: ansiString read getGraphicFile write FGraphicFile;
      property whichChip: word read getGraphic write FGraphic;
      property direction: TFacing read FDirection write FDirection;
      property animFrame: TAnimFrame read FFrame write FFrame;
      property transparent: boolean read getTransparent write FTransparent;
      property moveBlock: TEventMoveBlock read FMoveScript write FMoveScript;
      property moveType: TMoveType read FMoveType write FMoveType;
      property moveFrequency: byte read FMoveFrequency write FMoveFrequency;
      property startCondition: TStartCondition read FStartCondition write FStartCondition;
      property zOrder: byte read FEventHeight write FEventHeight;
      property is_barrier: boolean read FNoOverlap write FNoOverlap;
      property animType: TAnimType read FAnimType write FAnimType;
      property moveSpeed: byte read FMoveSpeed write FMoveSpeed;
      property eventScript: ansiString read getEventScript;
      property parent: TEvent read FParent;
      property compiledScript: tbtString read getCompiledScript;
      property hasScript: boolean read hasScriptFunction;
      property parseStack: TStack read FParseStack write FParseStack;
      property opcode: TRpgObjectList<TEventCommand> read FCommands;
      property len: word read getLength;
      property commands: TRpgObjectList<TEventCommand> read FCommands;
{$IFDEF ENGINE}
      property valid: boolean read isValid;
{$ENDIF}
   end;

   TEvent = class(TObject)
   private
      FName: ansiString;
      FID: word;
      FLocation: TPoint;
      FLength: smallint;
      FPages: array of TEventPage;
      FParent: TEventBlock;
      FCurrentPage: TEventPage;
      FCurrentlyPlaying: word;
      FLocked: boolean;
      FPageChanged: boolean;
      FDeleted: boolean;
{$IFDEF ENGINE}
      function getCurrentPage: TEventPage;
      function isCurrentlyPlaying: boolean; inline;
{$ENDIF}
      function getpage(x: word): TEventPage; inline;
      procedure setPage(x: word; input:TEventPage); inline;
      procedure setCurrentlyPlaying(value: boolean); inline;
   public
      constructor createGlobal(input: TStream; expected: word; parent: TEventBlock);
      constructor create(input: TStringStream; parent: TEventBlock); {$IFDEF ENGINE} overload;
      constructor create(newScript: string); overload; {$ENDIF}
      destructor Destroy; override;
{$IFDEF ENGINE}
      function isTile: boolean;
{$ENDIF}
      property page[x: word]: TEventPage read getPage write setPage; default;
      property location: TPoint read FLocation write FLocation;
      property id: word read FID write FID;
      property name: ansiString read FName write FName;
      property len: smallint read FLength write FLength;
      property parent: TEventBlock read FParent;
{$IFDEF ENGINE}
      property newCurrentPage: TEventPage read getCurrentPage;
      property playing: boolean read isCurrentlyPlaying write setCurrentlyPlaying;
{$ENDIF}
      property lastCurrentPage: TEventPage read FCurrentPage;
      property updated: boolean read FPageChanged;
      property locked: boolean read FLocked write FLocked;
      property deleted: boolean read FDeleted write FDeleted;
   end;

   TEventBlock = class(TObject)
   private
      FLength: word;
      FEvents: array of TEvent;
      FCompiler: TPSPascalCompiler;
      function getEvent(x: word): TEvent;
      {$IFDEF EDITOR}
      procedure setEvent(x: word; input: TEvent);
      {$ENDIF}
   public
      constructor create(const eventData: ansiString); overload;
      constructor create(const input: TStream); overload;
      destructor Destroy; override;
      function find(const id: word): TEvent;
      {$IFDEF ENGINE}
      procedure compileAll;
      {$ENDIF}

      property events[x: word]: TEvent read getEvent {$IFDEF EDITOR}write setEvent{$ENDIF}; default;
      property len: word read FLength;
      property compiler: TPSPascalCompiler read FCompiler write FCompiler;
   end;

const
   USESLIST: array[TUsesList] of ansiString = ('messages', 'party', 'sysdata', 'sound', 'graphics', 'menu', 'maps');

implementation

uses sysUtils, windows, //system libs
     fileIO, BER, commons, logs, {$IFDEF ENGINE}script_engine, script_interface, {$ENDIF}
     strtok; //3rd party libs

var
   GDatabase: TLcfDataBase;

const
   TRANSITION_TYPE_LIST: array[0..5] of ansiString = ('Teleport--Erase', 'Teleport--Show',
                    'Battle Start--Erase', 'Battle Start--Show', 'Battle End--Erase',
                    'Battle End--Show');
   C_TRANSITION_TYPE_LIST: array[0..5] of ansiString = ('trnMapEnter', 'trnMapExit',
                    'trnBattleStartErase', 'trnBattleStartShow', 'trnBattleEndErase',
                    'trnBattleEndShow');
   ERASE_TRANSITION_LIST: array[0..20] of ansiString = ('Fade Out', 'Whole Random Blocks',
                    'Random Block Up', 'Random Block Down', 'Blind Close', 'Hi-Low Stripe',
                    'Left-Right Stripe', 'Outside-Inside', 'Inside-Outside', 'Scroll Up',
                    'Scroll Down', 'Scroll Left', 'Scroll Right', 'Hi-Low Div',
                    'Left-Right Div', 'HiLow, LtRt Div', 'Zoom In', 'Twist',
                    'Ripple', 'Instant Erase', 'Do Not Erase');
   SHOW_TRANSITION_LIST: array[0..19] of ansiString = ('Fade Out', 'Whole Random Blocks',
                    'Random Block Up', 'Random Block Down', 'Blind Close', 'Hi-Low Stripe',
                    'Left-Right Stripe', 'Outside-Inside', 'Inside-Outside', 'Scroll Up',
                    'Scroll Down', 'Scroll Left', 'Scroll Right', 'Hi-Low Div',
                    'Left-Right Div', 'HiLow, LtRt Div', 'Zoom In', 'Twist',
                    'Ripple', 'Instant Show');
   C_TRANSITION_LIST: array[0..20] of ansiString = ('trnDefault', 'trnFade', 'trnBlocks',
                    'trnBlockUp', 'trnBlockDn', 'trnBlinds', 'trnStripeHiLo',
                    'trnStripeLR', 'trnOutIn', 'trnInOut', 'trnScrollU',
                    'trnScrollD', 'trnScrollL', 'trnScrollR',
                    'trnDivHiLow', 'trnDivLR', 'trnDivQuarters', 'trnZoom',
                    'trnTwist', 'trnRipple', 'trnNone');
   BGM_LIST: array[0..5] of ansiString = ('Battle', 'Victory', 'Boat', 'Ship', 'Airship',
                    'Game Over');
   C_BGM_LIST: array[TBgmTypes] of ansiString = ('bgmTitle', 'bgmBattle', 'bgmBossBattle',
                    'bgmVictory', 'bgmInn', 'bgmBoat', 'bgmShip', 'bgmAirship', 'bgmGameOver');
   EQUIPMENT_SLOTS: array[0..5] of ansiString = ('Weapons', 'Shield', 'Armor', 'Helmet',
                    'Other', '[All]');
   C_EQUIPMENT_SLOTS: array[0..5] of ansiString = ('weapon', 'shield', 'armor', 'helmet',
                    'relic', 'all');
   SYSTEM_SOUNDS: array[0..11] of ansiString = ('Menu Movement', 'Menu Decision', 'Menu Cancel',
                    'Buzzer', 'Battle Start', 'Esacpe', 'Enemy Attack', 'Enemy Damage',
                    'Hero Damage', 'Miss', 'Enemy Dies', 'Use Item');
   C_SYSTEM_SOUNDS: array[0..11] of ansiString = ('sfxCursor', 'sfxAccept', 'sfxCancel',
                    'sfxBuzzer', 'sfxBattleStart', 'sfxEscape', 'sfxEnemyAttack',
                    'sfxEnemyDamage', 'sfxAllyDamage', 'sfxEvade', 'sfxEnemyDies', 'sfxItemUsed');
   C_DIRECTIONS: array[0..3] of ansiString = ('dir_up', 'dir_right', 'dir_down', 'dir_left');
   C_WEATHER: array[0..4] of ansiString = ('we_off', 'we_rain', 'we_snow', 'we_fog', 'we_sand');
   C_IMAGE_EFFECTS: array[0..2] of ansiString = ('ie_none', 'ie_rotate', 'ie_wave');

   END_BLOCK: array[1..2] of integer = (20713, 20141);

function decodeMove(const data: array of integer; var pos: integer): string; forward;
procedure fillInEventInt(const expected: byte; out theResult: integer); forward;
procedure fillInEPageInt(const expected: byte; out theResult: integer); forward;
procedure fillInEConInt(const expected: byte; out theResult: integer); forward;
procedure fillInGlobalEConInt(const expected: byte; out theResult: integer); forward;
procedure fillInEMoveInt(const expected: byte; out theResult: integer); forward;

{ TEventBlock }

constructor TEventBlock.create(const eventData: ansiString);
var
   data: TStringStream;
   converter: intX80;
   i: integer;
begin
   inherited create;
   data := TStringStream.Create(eventData);
   converter := TBerConverter.Create(data);
   try
      FLength := converter.getData;
      setLength(FEvents, FLength);
      for i := 1 to FLength do
         FEvents[i - 1] := TEvent.create(data, self);
   finally
      data.Free;
   end; //end of overall try block
end;

{$IFDEF ENGINE}
procedure TEventBlock.compileAll;
var
   i: integer;
begin
   for i := 1 to FLength do
       FEvents[i - 1].lastCurrentPage.getCompiledScript;
   //end for
end;
{$ENDIF}

constructor TEventBlock.create(const input: TStream);
var
   converter: intX80;
   i: integer;
begin
   inherited Create;
   converter := TBerConverter.Create(input);
   FLength := converter.getData;
   setLength(FEvents, FLength);
   for i := 1 to FLength do
      FEvents[i - 1] := TEvent.createGlobal(input, i, self);
   //end for
end;

destructor TEventBlock.destroy;
var i: smallint;
begin
   for i := low(FEvents) to high(FEvents) do
      FEvents[i].Free;
   inherited;
end;

function TEventBlock.find(const id: word): TEvent;
var
  I: Integer;
begin
   result := nil;
   I := -1;
   repeat
      inc(i);
      if FEvents[i].id = id then
         result := FEvents[i];
   until (result <> nil) or (i = high(FEvents));
end;

function TEventBlock.getEvent(x: word): TEvent;
begin
   result := FEvents[x];
end;

{$IFDEF EDITOR}
procedure TEventBlock.setEvent(x: word; input: TEvent);
begin
   FEvents[x] := input;
end;
{$ENDIF}

{ TEvent }

constructor TEvent.create(input: TStringStream; parent: TEventBlock);
var
   pagesData: TStringStream;
   i: word;
   converter: intX80;
begin
   inherited create;
   FParent := parent;
   converter := TBerConverter.Create(input);
   try
      self.FID := converter.getData;
      FName := fileIO.getStrSec(1, input, fillInBlankStr);
      FLocation.x := getNumSec(2, input, fillInEventInt);
      FLocation.Y := getNumSec(3, input, fillInEventInt);
      pagesData := TStringStream.Create(getStrSec(5, input, fillInBlankStr));
      converter.read(pagesData);
      FLength := converter.getData;
      setLength(FPages, FLength);
      for i := 0 to high(FPages) do
         FPages[i] := TEventPage.Create(pagesData, i + 1, self);
   finally
      pagesData.Free;
   end;
   assert(peekAhead(input, 0));
   FCurrentPage := self[0];
end;

{$IFDEF ENGINE}
constructor TEvent.create(newScript: string);
begin
   inherited create;
   FParent := GGlobalEvents;
   setLength(FPages, 1);
   FPages[0] := TEventPage.create(newScript, 1, self);
   FPages[0].FOverride := true;
   FCurrentPage := self[0];
end;
{$ENDIF}

constructor TEvent.createGlobal(input: TStream; expected: word; parent: TEventBlock);
var
   converter: intX80;
begin
   inherited create;
   converter := TBerConverter.Create(input);
   if converter.getData <> expected then
      raise EParseMessage.create('Expected global event ' + intToStr(expected) + ' not found!');
   FID := expected;
   FParent := parent;
   FName := getStrSec(1, input, fillInBlankStr);
   setLength(FPages, 1);
   FPages[0] := TEventPage.createGlobal(input, self);
   FPages[0].FGlobal := true;
   FCurrentPage := self[0];
end;

destructor TEvent.destroy;
var i: smallint;
begin
   for i := low(FPages) to high(FPages) do
      FPages[i].Free;
   inherited;
end;

{$IFDEF ENGINE}
function TEvent.getCurrentPage: TEventPage;
var i: integer;
begin
   if FPages[0].FGlobal then
   begin
      result := FPages[0];
      Exit;
   end;

   result := nil;
   I := high(FPages);
   while (result = nil) and (i >= 0) do
   begin
      if FPages[i].valid then
         result := FPages[i];
      dec(i);
   end;
   FPageChanged := (FCurrentPage = result);
   FCurrentPage := result;
   if FPageChanged and assigned(FCurrentPage) then
      FCurrentPage.FOverrideSprite := false;
end;

function TEvent.isCurrentlyPlaying: boolean;
begin
   result := FCurrentlyPlaying > 0;
end;

function TEvent.isTile: boolean;
begin
   if assigned(lastCurrentPage) then
   begin
      if lastCurrentPage.FOverrideSprite then
         result := false
      else if (lastCurrentPage.FGraphicFile = '') then
         result := true
      else result := false;
   end else result := true;
end;
{$ENDIF}

function TEvent.getpage(x: word): TEventPage;
begin
   result := FPages[x];
end;

procedure TEvent.setCurrentlyPlaying(value: boolean);
begin
   if value then
      inc(FCurrentlyPlaying)
   else FCurrentlyPlaying := max(FCurrentlyPlaying - 1, 0);
end;

procedure TEvent.setPage(x: word; input: TEventPage);
begin
   FPages[x] := input;
end;

{ TEventMoveBlock }

constructor TEventMoveBlock.Create(input: TStream);
var
   moveString: ansiString;
   loop: boolean;
begin
   inherited create;
   if not peekAhead(input, $29) then
      raise EParseMessage.create('Event page movement block not found!');
   TBerConverter.create(input); //skip size info;

   skipSec($B, input);
   moveString := getStrSec($C, input, fillInBlankStr);
   if moveString <> '' then
   begin
      loop := getChboxSec($15, input, fillInEMoveInt);
      FOrder := TMoveOrder.Create(moveString, loop);
   end else skipSec($15, input);
   FIgnore := getChboxSec($16, input, fillInZeroInt);
   assert(peekAhead(input, 0));
end;

destructor TEventMoveBlock.Destroy;
begin
   FOrder.Free;
   inherited;
end;

{ TEventPage }

constructor TEventPage.create(var input: TStringStream; const expected: word; parent: TEvent);
var
   conditionStream: TStringStream;
   opStream: TStringStream;
   dummy: integer;
begin
   inherited create;
   FParent := parent;
   if not peekAhead(input, expected) then
      raise EParseMessage.create('Expected event page ' + intToStr(expected) + 'not found!');
   FCommands := TRpgObjectList<TEventCommand>.Create(true);

   FID := expected;
   conditionStream := TStringStream.Create(getStrSec(2, input, fillInBlankStr));
   try
      FConditions := TEventConditions.create(conditionStream);
   finally
      conditionStream.Free;
   end;
   FGraphicFile := getStrSec($15, input, fillInBlankStr);
   FGraphic := getNumSec($16, input, fillInEPageInt);
   FDirection := TFacing(getNumSec($17, input, fillInEPageInt));
   FFrame := TAnimFrame(getNumSec($18, input, fillInEPageInt));
   FTransparent := getChboxSec($19, input, fillInEPageInt);
   FMoveType := TMoveType(getNumSec($1F, input, fillInEPageInt));
   FMoveFrequency := getNumSec($20, input, fillInEPageInt);
   FStartCondition := TStartCondition(getNumSec($21, input, fillInEPageInt));
   FEventHeight := getNumSec($22, input, fillInEPageInt);
   FNoOverlap :=  getChboxSec($23, input, fillInEPageInt);
   FAnimType := TAnimType(getNumSec($24, input, fillInEPageInt));
   FMoveSpeed := getNumSec($25, input, fillInEPageInt);
   FMoveScript := TEventMoveBlock.Create(input);

//# 33 ought to be the size of section 34.  (Why so redundant?)
   dummy := getNumSec($33, input, fillInEpageInt);
   FEventScript := getStrSec($34, input, fillInBlankStr);
   assert (dummy = length(FEventScript));

   opStream := TStringStream.Create(FEventScript);
   try
      if length(FEventScript) > 0 then
      repeat
         FCommands.Add(TEventCommand.create(opStream, self));
      until opStream.Position >= opStream.Size - 1;
   finally
      opStream.free;
   end;

//ends with a 00?
   assert(peekAhead(input, 0));
   FEventText := '';
   FCompiledScript := '';
   FScriptText := '';
   FParseStack := TStack.Create;
   FLabelList := '';
end;

constructor TEventPage.createGlobal(input: TStream; parent: TEvent);
var
   dummy: integer;
   opStream: TStringStream;
begin
   inherited create;
   FParent := parent;
   FCommands := TRpgObjectList<TEventCommand>.Create(true);
   FStartCondition := TStartCondition(getNumSec($B, input, fillInEPageInt));
   FConditions := TEventConditions.createGlobal(input);
//# 15 ought to be the size of section 34.  (Why so redundant?)
   dummy := getNumSec($15, input, fillInEpageInt);
   FEventScript := getStrSec($16, input, fillInBlankStr);
   assert (dummy = length(FEventScript));

   opStream := TStringStream.Create(FEventScript);
   try
      if length(FEventScript) > 0 then
      repeat
         FCommands.Add(TEventCommand.create(opStream, self));
      until opStream.Position >= opStream.Size - 1;
   finally
      opStream.free;
   end;
//ends with a 00?
   assert(peekAhead(input, 0));
   FParseStack := TStack.Create;
end;

constructor TEventPage.create(input: ansiString; const expected: word; parent: TEvent);
begin
   inherited create;
   FParent := parent;
   FCommands := TRpgObjectList<TEventCommand>.Create(true);
   FID := expected;
   FConditions := TEventConditions.create(nil);
   FEventText := '';
   FCompiledScript := '';
   FScriptText := input;
   FParseStack := TStack.Create;
   FLabelList := '';
end;

destructor TEventPage.Destroy;
var
   i: integer;
begin
   FParseStack.Free;
   FConditions.Free;
   FMoveScript.Free;
   FCommands.free;
   inherited;
end;

function TEventPage.getCompiledScript: tbtString;
begin
   if FCompiledScript <> '' then
   begin
      result := FCompiledScript;
      Exit;
   end;

   if not assigned(parent.parent.FCompiler) then
      raise EFatalError.create('Compiler not assigned!');
   result := getEventScript;
   if not parent.parent.compiler.Compile(result) then
   begin
      logText(unicodeString(result));
      logText(unicodeString(parent.parent.compiler.Msg[0].MessageToString));
      msgBox(unicodeString(parent.parent.compiler.Msg[0].MessageToString), 'Error');
      raise EFatalError.create('Could not compile event script for page ' + intToSTR(FID));
   end;

   parent.parent.compiler.getOutput(result);
   FCompiledScript := result;
end;

function TEventPage.getEventScript: ansiString;
var
   i: integer;
   j: TUsesList;
   used: ansiString;
   enumerator: TEventCommand;
begin
   if FScriptText <> '' then
   begin
      result := FScriptText;
      Exit;
   end;

   FUseList := [];
   used := '';
   result := 'begin' + LFCR + 'with rsys do' + LFCR + 'begin' + LFCR;
   TEventCommand.FCorrection := 0;
   FLoopDepth := 0;
   for enumerator in FCommands do
   begin
      if (enumerator.FOpcode = 22010) or (enumerator.FOpcode = 20721 ) or (enumerator.FOpcode = 20731) then
         stripLastSem(result);
      result := result + enumerator.getScript;
   end;
   //end FOR
   if FLabelList <> '' then
      result := 'label ' + FLabelList + ';' + LFCR + result;
   for i := 1 to high(FIndexList) do
      if FIndexList[i] then
      begin
         if used <> '' then
            used := used + ', ';
         used := used + 'i' + ansiString(intToStr(i));
      end;
   if used <> '' then
      result := 'var ' + used + ': integer; //index variable(s)' + LFCR + result;

   used := '';
   for j := low(TUsesList) to high(TUsesList) do
      if j in FUseList then
      begin
         if used <> '' then
            used := used + ', ';
         used := used + USESLIST[j];
      end;
   //end FOR
   if used <> '' then
      result := 'uses ' + used + ';' + LFCR + LFCR + result;
   result := 'program script;' + LFCR + result;
   FScriptText := result;
end;

function TEventPage.getGraphic: word;
begin
   if not FOverrideSprite then
      result := FGraphic
   else result := FOverrideGraphic;
end;

function TEventPage.getGraphicFile: ansiString;
begin
   if not FOverrideSprite then
      result := FGraphicFile
   else result := FOverrideFile;
end;

function TEventPage.getLength: word;
begin
   result := FCommands.high;
end;

function TEventPage.getTransparent: boolean;
begin
   if not FOverrideSprite then
      result := FTransparent
   else result := FOverrideTransparency;
end;

function TEventPage.hasScriptFunction: boolean;
begin
   result := FOverride or (FCommands.high > 0);
end;

{$IFDEF ENGINE}
function TEventPage.isValid: boolean;
begin
   result := FConditions.valid;
end;
{$ENDIF}

procedure TEventPage.overrideSprite(filename: ansiString; index: byte; transparent: boolean);
begin
   FOverrideSprite := true;
   FOverrideFile := filename;
   FOverrideGraphic := index;
   FOverrideTransparency := transparent;
end;

procedure TEventPage.stripLastSem(var data: ansiString);
var
   I: Integer;
begin
   i := length(data) - 1;
   while (i > 0) and (data[i] <> ';') do
      dec(i);
   if i <> 0 then
      delete(data, i, 1);
end;

procedure TEventPage.use(value: TUsesList);
begin
   include(FUseList, value);
end;

{ TEventConditions }

constructor TEventConditions.create(input: TStringStream);
var dummy: byte;
begin
   inherited Create;
   if input = nil then
      Exit;

   dummy := getNumSec(1, input, fillInEPageInt);
   if dummy > 0 then
   begin
      FConditions[switch1] := (dummy and 1 = 1);
      FConditions[switch2] := (dummy and 2 = 2);
      FConditions[variable1] := (dummy and 4 = 4);
      FConditions[item] := (dummy and 8 = 8);
      FConditions[hero] := (dummy and $10 = $10);
      FConditions[timer] := (dummy and $20 = $20);
      FConditions[timer2] := (dummy and $40 = $40);
   end;
   FSwitch1 := getNumSec(2, input, fillInEConInt);
   FSwitch2 := getNumSec(3, input, fillInEConInt);
   FVariable := getNumSec(4, input, fillInEConInt);
   FVarValue := getNumSec(5, input, fillInEConInt);
   FItem := getNumSec(6, input, fillInEConInt);
   FHero := getNumSec(7, input, fillInEConInt);
   FClock := getNumSec(8, input, fillInEConInt);
   FClock2 := getNumSec(9, input, fillInEConInt);
   FVarOperator := getNumSec($A, input, fillInEConInt);
   if not (peekAhead(input, 0)) then
      raise EParseMessage.CreateFmt('Unknown section %d found at end of TEventConditions', [peekAhead(input)]);
end;

constructor TEventConditions.createGlobal(input: TStream);
begin
   inherited Create;
   FConditions[switch1] := getChboxSec($C, input, fillInZeroInt);
   FSwitch1 := getNumSec($D, input, fillInGlobalEConInt);
end;

{$IFDEF ENGINE}
function TEventConditions.evaluate: boolean;

   function itemPresent(value: word): boolean;
   var
      I: Integer;
   begin
      result := false;
      if GParty.inventory.indexOf(value) <> -1 then
         result := true
      else begin
         for I := 1 to 4 do
            if GParty.hero[i].equipped(value) then
               result := true;
            //end if
         //end for
      end;
   end;

   function heroPresent(value: byte): boolean;
   var
      I: Integer;
   begin
      result := false;
      for I := 1 to MAXPARTYSIZE do
         if (GParty.hero[i] <> GScriptEngine.hero[0]) and (GParty.hero[i].template.id = value) then
            result := true;
         //end if
      //end for
   end;

begin
   result := (((not FConditions[switch1]) or (GSwitches[FSwitch1])) and
              ((not FConditions[switch2]) or (GSwitches[FSwitch2])) and
              ((not FConditions[variable1]) or (GVariables[FVariable] >= FVarValue)) and
              ((not FConditions[item]) or (itemPresent(FItem))) and
              ((not FConditions[hero]) or (heroPresent(FHero))) and
              ((not FConditions[timer]) or (not GScriptEngine.timer.active) or (GScriptEngine.timer.time <= FClock)));
end;
{$ENDIF}

{ TEventCommand }

constructor TEventCommand.create(input: tstream; parent: TEventPage);
var
   converter: intx80;
   I: Integer;
begin
   FParent := parent;
   converter := TBerConverter.Create(input);
   FOpcode := converter.getData;
   FDepth := getNext(input);
   if not peekAhead(input, 0) then
      FName := getString(input)
   else
      setLength(FName, 0);
   converter.read(input);
   setLength(FData, converter.getData);
   if length(FData) > 0 then
   begin
      for I := 1 to length(FData) do
      begin
         converter.read(input);
         FData[i-1] := converter.getData;
      end;
   end;
end;

{$IFDEF EDITOR}
function TEventCommand.eventDeref(const data: integer): ansiString;
begin
   case data of
      10001: result := 'Hero';
      10002: result := 'Boat';
      10003: result := 'Ship';
      10004: result := 'Airship';
      10005: result := 'this Event';
      else
         result := parent.parent.parent.find(data).name;
      //end of ELSE case
   end;
end;
{$ELSE}
function TEventCommand.eventDeref(const data: integer): ansiString;
begin
   case data of
      10001: result := 'party';
{      10002: result := 'vehicle[1]';
      10003: result := 'vehicle[2]';
      10004: result := 'vehicle[3]';}
      10002: result := 'boat';
      10003: result := 'ship';
      10004: result := 'airship';
      10005: result := 'thisEvent';
      else
         result := 'event[' + intToStr(data) + ']';
      //end of ELSE case
   end;
end;
{$ENDIF}

function TEventCommand.getNextCommand: TEventCommand;
var
   I: Integer;
begin
   i := -1;
   repeat
      inc(i);
   until (i = parent.FCommands.high) or (parent.FCommands[i] = self);
   if i = parent.FCommands.high then
      result := nil
   else result := parent.FCommands[i + 1];
end;

{function TEventCommand.getPreviousCommand: TEventCommand;
var
   I: Integer;
begin
   i := -1;
   repeat
      inc(i);
   until (i = high(parent.FCommands)) or (parent.FCommands[i] = self);
   if i = 0 then
      result := nil
   else result := parent.FCommands[i - 1];
end;}

function TEventCommand.getScript: ansiString;
var
   i, j: integer;
   continuing: boolean;
   dummy: string;
   effect: boolean;
   stackFrame: TCaseStackFrame;

   function caseScan(out count: integer): ansiString;
   var
      iterator: TEventCommand;
   begin
      iterator := next;
      result := '';
      count := 0;
      repeat
         if iterator.FDepth = self.FDepth then
         begin
            assert((iterator.FOpcode = 20140) or ((iterator.FOpcode = 20141)));
            if iterator.FOpcode = 20140 then
            begin
               j := 1;
               repeat
                  i := inString(unicodeString(iterator.FName), '''', j);
                  j := i + 2;
                  if i <> -1 then
                     strInsertAnsi(iterator.FName, '''', i);
               until i = -1;
               //add separator
               if result <> '' then
                  result := result + #3;
               if iterator.FName <> '' then
                  result := result + iterator.FName
               else result := result + ' ';
            end;
            inc(count);
         end;
         iterator := iterator.next;
      until iterator.FOpcode = 20141;
   end;

   function indent(depth: byte): ansiString;
   var i: byte;
   begin
      result := '   ';
      if (FOpcode = 10) and (depth > 0) then
         dec(depth);
      for I := 1 to depth do
         result := result + '   ';
      //end for
      for I := 1 to FCorrection do
         result := result + '   ';
      //end for
   end;

   procedure use(value: TUsesList);
   begin
      FParent.use(value);
   end;

begin
   result := '';
   continuing := false;
   for I := 1 to high(END_BLOCK) do
      if FOpcode = END_BLOCK[i] then
         dec(FCorrection);
      //end if
   //end for
   if FOpcode <> 0 then
      result := result + indent(FDepth);
   case FOpcode of
      0: result := result + 'end; //end of WITH block' + LFCR + 'end.';
      10: //End of a block
         result := result + 'end;';
{MESSAGE BOX CODE}
{$REGION Message box code}
      10110, 20110: //show message
      begin
         use(uses_messages);
         j := 1;
         repeat
            i := inStringA(FName, '''', j);
            j := i + 2;
            if i <> -1 then
               strInsertAnsi(FName, '''', i);
         until i = -1;
         if FOpcode = 10110 then
            result := result + 'showMessage(''' + FName
         else
            result := result + ' ' + FName;
         if (self.next <> nil) and (self.next.FOpcode = 20110) then
            continuing := true
         else result := result + ''');';
      end;
      10120:
      begin
         use(uses_messages);
         result := result + 'messageOptions(';
         case boolean(FData[0]) of
            false: result := result + 'true, ';
            true: result := result + 'false, ';
         end;
         case FData[1] of
            0: result := result + 'mb_top, ';
            1: result := result + 'mb_middle, ';
            2: result := result + 'mb_bottom, ';
         end;
         case boolean(FData[2]) of
            false: result := result + 'false, ';
            true: result := result + 'true, ';
         end;
         case boolean(FData[3]) of
         //switching true and false.  Original is "allow events to continue?"
         //my version is "modal?"
            false: result := result + 'true';
            true: result := result + 'false';
         end;
         result := result + ');';
      end;
      10130: //set message face
      begin
         use(uses_messages);
         if FName = '' then
            result := result + 'clearPortrait;'
         else
         begin
            result := result + 'setPortrait(''';
            result := result + FName + ''', ' + ansiString(intToStr(FData[0])) + ', ';
            case boolean(FData[1]) of
               false: result := result + 'false, ';
               true: result := result + 'true, ';
            end;
            case boolean(FData[2]) of
               false: result := result + 'false);';
               true: result := result + 'true);';
            end;
            //end if
         end; //end else
      end;
      10140: //CASE
      begin
         use(uses_messages);
         result := result + 'case showChoice(''' + caseScan(i) + ''', ' + ansiString(intToStr(FData[0])) + ') of';
         assert(self.next.FOpcode = 20140);
         self.FParent.parseStack.Push(TCaseStackFrame.Create(FDepth, i, TCancelHandler(FData[0]) = cancelElse));
         inc(FCorrection);
      end;
      20140: //case handlers
      begin
         stackFrame := FParent.parseStack.peek;
         if (stackFrame.current + 1 = stackFrame.count) and (stackFrame.elseCase) then
            result := result + 'else'
         else
         begin
            result := result + ansiString(intToStr(stackFrame.current)) + ':';
            stackFrame.current := stackFrame.current + 1;
         end;
         result := result + LFCR + indent(FDepth) + 'begin';
      end;
      20141: //end of case block
      begin
         result := result + 'end; //end of CASE block';
         stackFrame := FParent.parseStack.Pop;
         stackFrame.Free;
      end;
      10150: //input number
      begin
         use(uses_messages);
         result := result + 'variable[' + ansiString(intToStr(FData[1])) + '] := inputNumber(' + ansiString(intToStr(FData[0])) + ');';
      end;
{$ENDREGION}
{INTERNAL LOGIC}
{$REGION INTERNAL LOGIC}
{SWITCH/VAR/TIMER}
{$REGION SWITCHVAR}
      10210: //change switch
      begin
         case FData[0] of
            0: dummy := 'switch[' + intToStr(FData[1]) + ']';
            1:
            begin
               parent.FIndexList[FDepth + 1] := true;
               result := result + 'for i' + ansiString(intToStr(FDepth + 1)) + ' := ' + ansiString(intToStr(FData[1]));
               case boolean(FData[2] >= FData[1]) of
                  true: result := result + ' to ';
                  false: result := result + ' downto ';
               end;
               result := result + ansiString(intToStr(FData[2])) + ' do' + LFCR;
               result := result + indent(FDepth + 1);
               dummy := 'switch[i' + intToStr(FDepth + 1) + ']';
            end;
            2: dummy := 'switch[variable[' + intToStr(FData[1]) + ']]';
            else raise EParseMessage.create('Unknown switch data 0 case x' + intToHex(FData[0], 2) + '!');
         end;
         result := result + ansiString(dummy) + ' := ';
         case FData[3] of
            0: result := result + 'true;';
            1: result := result + 'false;';
            2: result := result + 'not ' + ansiString(dummy) + ';';
            else raise EParseMessage.create('Unknown switch data 3 case x' + intToHex(FData[3], 2) + '!');
         end;
      end;
      10220: //Variable
      begin
         case FData[0] of
            0: dummy := 'variable[' + intToStr(FData[1]) + ']';
            1:
            begin
               parent.FIndexList[FDepth + 1] := true;
               result := result + 'for i' + ansiString(intToStr(FDepth + 1)) + ' := ' + ansiString(intToStr(FData[1]));
               case boolean(FData[2] >= FData[1]) of
                  true: result := result + ' to ';
                  false: result := result + ' downto ';
               end;
               result := result + ansiString(intToStr(FData[2])) + ' do' + LFCR;
               result := result + indent(FDepth + 1);
               dummy := 'variable[i' + intToStr(FDepth + 1) + ']';
            end;
            2: dummy := 'variable[variable[' + intToStr(FData[1]) + ']]';
            else raise EParseMessage.create('Unknown variable data 0 case x' + intToHex(FData[0], 2) + '!');
         end;
         result := result + ansiString(dummy) + ' := ';
         case FData[3] of
            0: ; //assignment
            1: result := result + ansiString(dummy) + ' + ';
            2: result := result + ansiString(dummy) + ' - ';
            3: result := result + ansiString(dummy) + ' * ';
            4: result := result + ansiString(dummy) + ' / ';
            5: result := result + ansiString(dummy) + ' mod ';
         end;
         case FData[4] of
            0: result := result + ansiString(intToStr(FData[5])) + ';';
            1: result := result + 'variable[' + ansiString(intToStr(FData[5])) + '];';
            2: result := result + 'variable[variable[' + ansiString(intToStr(FData[5])) + ']];';
            3: result := result + 'random( ' + ansiString(intToStr(FData[5])) + ', ' + ansiString(intToStr(FData[6])) + ');';
            4:
            begin
               use(uses_system);
               result := result + ' heldItems(' + ansiString(intToStr(FData[5])) + ', ';
               case FData[6] of
                  0: result := result + ' false';
                  1: result := result + ' true';
               end;
               result := result + ');';
            end;
            5:
            begin
               use(uses_system);
               result := result + 'hero['+ ansiString(intToStr(FData[5])) + '].';
               case FData[6] of
                  0: result := result + 'level;';
                  1: result := result + 'exp;';
                  2: result := result + 'hp;';
                  3: result := result + 'mp;';
                  4: result := result + 'maxHp;';
                  5: result := result + 'maxMp;';
                  6..9: result := result + 'stat[' + ansiString(intToStr(FData[6] - 5)) + '];';
                  10..14: result := result + 'equipment[' + ansiString(intToStr(FData[6] - 9)) + '];';
               end;
            end;
            6:
            begin
               result := result + eventDeref(FData[5]) + '.';
               case FData[6] of
                  0: result := result + 'map;';
                  1: result := result + 'X;';
                  2: result := result + 'Y;';
                  3: result := result + 'facing;';
                  4: result := result + 'screenX;';
                  5: result := result + 'screenY;';
               end;
            end;
            7:
            begin
               case FData[5] of
                  0: result := result + 'money;';
                  1: result := result + 'timer.time;';
                  2: result := result + 'partySize;';
                  3: result := result + 'saveCount;';
                  4: result := result + 'battles;';
                  5: result := result + 'victories;';
                  6: result := result + 'losses;';
                  7: result := result + 'flees;';
                  8: result := result + 'bgm.position;';
               end;
            end; //end of case 7
         end; //end of FData[4] block
      end; //end of variable block
      10230: //timer option
      begin
         result := result + 'timer.';
         case FData[0] of
            0:
            begin
               result := result + 'time := ';
               case boolean(FData[1]) of
                  false: result := result + ansiString(intToStr(FData[2])) + ';';
                  true: result := result + 'variable[' + ansiString(intToStr(FData[2])) + '];';
               end;
            end;
            1:
            begin
               result := result + 'start(';
               case boolean(FData[3]) of
                  true: result := result + 'true, ';
                  false: result := result + 'false, ';
               end;
               case boolean(FData[4]) of
                  true: result := result + 'true);';
                  false: result := result + 'false);';
               end;
            end;
            2: result := result + 'pause;';
         end;
      end;
{$ENDREGION}
      10310: //change money
      begin
         result := result + 'money := ';
         case boolean(FData[0]) of
            true: result := result + 'money - ';
            false: result := result + 'money + ';
         end;
         case boolean(FData[1]) of
            true: result := result + 'variable[' + ansiString(intToStr(FData[2])) + '];';
            false: result := result + ansiString(intToStr(FData[2])) + ';';
         end;
      end;
      10320: //change inventory
      begin
         use(uses_system);
         case boolean(FData[0]) of
            true: result := result + 'removeItem(';
            false: result := result + 'addItem(';
         end;
         case boolean(FData[1]) of
            true: result := result + 'variable[' + ansiString(intToStr(FData[2])) + '], ';
            false: result := result + ansiString(intToStr(FData[2])) + ', ';
         end;
         case boolean(FData[3]) of
            true: result := result + 'variable[' + ansiString(intToStr(FData[4])) + ']);';
            false: result := result + ansiString(intToStr(FData[4])) + ');';
         end;
      end;
      10330: //change party
      begin
         use(uses_system);
         case boolean(FData[0]) of
            true: result := result + 'heroLeave(';
            false: result := result + 'heroJoin(';
         end;
         case boolean(FData[1]) of
            true: result := result + 'Var[' + ansiString(intToStr(FData[2])) + ']);';
            false: result := result + ansiString(intToStr(FData[2])) + ');';
         end;
      end;
      10410: //change EXP
      begin
         use(uses_system);
         if boolean(FData[2]) = false then
         begin
            result := result + 'levelGainNotify := ';
            case boolean(FData[5]) of
               false: result := result + 'false;';
               true: result := result + 'true;';
            end;
            result := result + LFCR + indent(FDepth);
         end;

         case boolean(FData[2]) of
            false: result := result + 'addExp(';
            true: result := result + 'removeExp(';
         end;
         case FData[0] of
            0: result := result + '-1, ';
            1: result := result + ansiString(intToStr(FData[1])) + ', ';
            2: result := result + 'variable[' + ansiString(intToStr(FData[1])) + '], ';
         end;
         case boolean(FData[3]) of
            false: result := result + ansiString(intToStr(FData[4])) + ');';
            true: result := result + 'variable[' + ansiString(intToStr(FData[4])) + ']);';
         end;
      end;
      10420: //change level
      begin
         use(uses_system);
         if boolean(FData[2]) = false then
         begin
            result := result + 'levelGainNotify := ';
            case boolean(FData[5]) of
               false: result := result + 'false;';
               true: result := result + 'true;';
            end;
            result := result + LFCR + indent(FDepth);
         end;

         case boolean(FData[2]) of
            false: result := result + 'addLevels(';
            true: result := result + 'removeLevels(';
         end;
         case FData[0] of
            0: result := result + '-1, ';
            1: result := result + ansiString(intToStr(FData[1])) + ', ';
            2: result := result + 'variable[' + ansiString(intToStr(FData[1])) + '], ';
         end;
         case boolean(FData[3]) of
            false: result := result + ansiString(intToStr(FData[4])) + ');';
            true: result := result + 'Var[' + ansiString(intToStr(FData[4])) + ']);';
         end;
      end;
      10430: //Change Ability
      begin
         case FData[0] of
            0:
            begin
               parent.FIndexList[FDepth + 1] := true;
               result := result + 'for i' + ansiString(intToStr(FDepth + 1)) + ' := 1 to 4 do'
                      + LFCR + indent(FDepth + 1);
               dummy := 'party[i' + intToStr(FDepth + 1)+ '].';
            end;
            1: dummy := 'hero[' + intToStr(FData[1]) + '].';
            2: dummy := 'hero[variable[' + intToStr(FData[1]) + ']].';
         end;
         case FData[3] of
            0: dummy := dummy + 'maxHp ';
            1: dummy := dummy + 'maxMp ';
            2: dummy := dummy + 'attack ';
            3: dummy := dummy + 'defense ';
            4: dummy := dummy + 'mind ';
            5: dummy := dummy + 'agility ';
         end;
         result := result + ansiString(dummy) + ' := ' + ansiString(dummy);
         case boolean(FData[2]) of
            false: result := result + '+ ';
            true: result := result + '- ';
         end;
         case boolean(FData[4]) of
            false: result := result + ansiString(intToStr(FData[5])) + ';';
            true: result := result + 'variable[' + ansiString(intToStr(FData[5])) + '];';
         end;
      end;
      10440: //change skill
      begin
         case FData[0] of
            0:
            begin
               parent.FIndexList[FDepth + 1] := true;
               result := result + 'for i' + ansiString(intToStr(FDepth + 1)) + ' := 1 to 4 do'
                      + LFCR + indent(FDepth + 1) + 'party[i' + ansiString(intToStr(FDepth + 1))+ '].';
            end;
            1: result := result + 'hero[' + ansiString(intToStr(FData[1])) + '].';
            2: result := result + 'hero[variable[' + ansiString(intToStr(FData[1])) + ']].';
         end;
         result := result + 'skill[';
         case boolean(FData[3]) of
            false: result := result + ansiString(intToStr(FData[4])) + '] := ';
            true: result := result + 'variable[' + ansiString(intToStr(FData[4])) + ']] := ';
         end;
         case boolean(FData[2]) of
            false: result := result + 'true;';
            true: result := result + 'false;';
         end;
      end;
      10450: //change equipment
      begin
         case FData[0] of
            0:
            begin
               parent.FIndexList[FDepth + 1] := true;
               result := result + 'for i' + ansiString(intToStr(FDepth + 1)) + ' := 1 to 4 do'
                      + LFCR + indent(FDepth + 1) + 'party[i' + ansiString(intToStr(FDepth + 1)) + '].';
            end;
            1: result := result + 'hero[' + ansiString(intToStr(FData[1])) + '].';
            2: result := result + 'hero[variable[' + ansiString(intToStr(FData[1])) + ']].';
         end;
         case boolean(FData[2]) of
            false:
            begin
               result := result + 'equip(';
               case boolean(FData[3]) of
                  false: result := result + ansiString(intToStr(FData[4])) + ');';
                  true: result := result + 'variable[' + ansiString(intToStr(FData[4])) + ']);';
               end;
            end; //end false
            true: result := result + 'unequip(' + C_EQUIPMENT_SLOTS[FData[3]] + ');';
         end; //end case
      end;
      10460: //change HP
      begin
         if boolean(FData[2]) = true then
         begin
            result := result + 'deathPossible := ';
            case boolean(FData[5]) of
               false: result := result + 'false;';
               true: result := result + 'true;';
            end;
            result := result + LFCR + indent(FDepth);
         end;

         case FData[0] of
            0:
            begin
               parent.FIndexList[FDepth + 1] := true;
               result := result + 'for i' + ansiString(intToStr(FDepth + 1)) + ' := 1 to 4 do'
                      + LFCR + indent(FDepth + 1);
               dummy := 'party[i' + intToStr(FDepth + 1)+ '].';
            end;
            1: dummy := 'hero[' + intToStr(FData[1]) + '].';
            2: dummy := 'hero[variable[' + intToStr(FData[1]) + ']].';
         end;
         dummy := dummy + 'hp ';
         result := result + ansiString(dummy) + ' := ' + ansiString(dummy);
         case boolean(FData[2]) of
            false: result := result + '+ ';
            true: result := result + '- ';
         end;
         case boolean(FData[3]) of
            false: result := result + ansiString(intToStr(FData[4])) + ';';
            true: result := result + 'variable[' + ansiString(intToStr(FData[4])) + '];';
         end;
      end;
      10470: //change MP
      begin
         case FData[0] of
            0:
            begin
               parent.FIndexList[FDepth + 1] := true;
               result := result + 'for i' + ansiString(intToStr(FDepth + 1)) + ' := 1 to 4 do'
                      + LFCR + indent(FDepth + 1);
               dummy := 'party[i' + intToStr(FDepth + 1)+ '].';
            end;
            1: dummy := 'hero[' + intToStr(FData[1]) + '].';
            2: dummy := 'hero[variable[' + intToStr(FData[1]) + ']].';
         end;
         dummy := dummy + 'mp ';
         result := result + ansiString(dummy) + ':= ' + ansiString(dummy);
         case boolean(FData[2]) of
            false: result := result + '+ ';
            true: result := result + '- ';
         end;
         case boolean(FData[3]) of
            false: result := result + ansiString(intToStr(FData[4])) + ';';
            true: result := result + 'variable[' + ansiString(intToStr(FData[4])) + '];';
         end;
      end;
      10480: //change status
      begin
         case FData[0] of
            0:
            begin
               parent.FIndexList[FDepth + 1] := true;
               result := result + 'for i' + ansiString(intToStr(FDepth + 1)) + ' := 1 to 4 do'
                      + LFCR + indent(FDepth + 1) + 'party[i' + ansiString(intToStr(FDepth + 1)) + '].';
            end;
            1: result := result + 'hero[' + ansiString(intToStr(FData[1])) + '].';
            2: result := result + 'hero[variable[' + ansiString(intToStr(FData[1])) + ']].';
         end;
         result := result + 'condition[' + ansiString(intToStr(FData[3])) + '] := ';
         case boolean(FData[2]) of
            true: result := result + ' true;';
            false: result := result + ' false;';
         end;
      end;
      10490: //Full heal
      begin
         case FData[0] of
            0:
            begin
               parent.FIndexList[FDepth + 1] := true;
               result := result + 'for i' + ansiString(intToStr(FDepth + 1)) + ' := 1 to 4 do'
                      + LFCR + indent(FDepth + 1) + 'party[i' + ansiString(intToStr(FDepth + 1)) + '].';
            end;
            1: result := result + 'hero[' + ansiString(intToStr(FData[1])) + '].';
            2: result := result + 'hero[variable[' + ansiString(intToStr(FData[1])) + ']].';
         end;
         result := result + 'fullheal;';
      end;
      10500: //take damage
      begin
         if boolean(FData[6]) = true then
            result := result + 'variable[' + ansiString(intToStr(FData[7])) + '] := ';
         case FData[0] of
            0:
            begin
               result := result + 'party.takeDamage(';
            end;
            1: result := result + 'hero[' + ansiString(intToStr(FData[1])) + '].takeDamage(';
            2: result := result + 'hero[variable[' + ansiString(intToStr(FData[1])) + ']].takeDamage(';
         end;
         for I := 3 to 6 do
         begin
            result := result + ansiString(intToStr(FData[i]));
            if i < 6 then
               result := result + ', ';
            //end if
         end;
         result := result + ');';
      end;
      10610: //change hero's name
         result := result + 'Hero[' + ansiString(intToStr(FData[0])) + '].name := ''' + FName + ''';';
      10620: //change hero's class
         result := result + 'Hero[' + ansiString(intToStr(FData[0])) + '].charClass := ''' + FName + ''';';
{$ENDREGION}
{SYSTEM GRAPHICS & SOUND}
{$REGION SYSTEM GRAPHICS & SOUND}
      10630: //change hero graphic
      begin
         result := result + 'hero[' + ansiString(intToStr(FData[0])) + '].setSprite('''
                + FName + ''', ' + ansiString(intToStr(FData[1])) + ', ';
         case boolean(FData[2]) of
            true: result := result + 'true);';
            false: result := result + 'false);';
         end;
      end;
      10640: //change hero's portrait
         result := result + 'hero[' + ansiString(intToStr(FData[0])) + '].setPortrait('''
                 + FName + ''', ' + ansiString(intToStr(FData[1] + 1)) + ');';
      10650: //change vehicle graphic
      begin
         result := result + 'vehicle[' + ansiString(intToStr(FData[0] + 1)) + ']';
         result := result + '.setSprite(''' + FName + ''', ' + ansiString(intToStr(FData[1] + 1)) + ');';
      end;
      10660: //Change BGM
      begin
         use(uses_system);
         result := result + 'setSystemMusic(';
         result := result + C_BGM_LIST[tBgmTypes(FData[0])] + ', ''';
         if FData[1] = 1000 then
            result := result + '(OFF)'');'
         else
            result := result + FName + ''');';
         //end if
      end;
      10670: //change system sound
      begin
         use(uses_system);
         result := result + 'setSystemSound(' + C_SYSTEM_SOUNDS[FData[0]] + ', ''';
         if FData[1] = 1000 then
            result := result + '(OFF)'');'
         else
            result := result + FName + ''');';
         //end if
      end;
      10680: //change system graphic
      begin
         use(uses_system);
         result := result + 'setSkin(''' + FName + ''');';
      end;

      10690: //screen transitions
      begin
         use(uses_maps);
         result := result + 'setTransition(';
         result := result + C_TRANSITION_TYPE_LIST[FData[0]] + ', ';
         result := result + C_TRANSITION_LIST[FData[1]] + ');';
      end;
{$ENDREGION}
{PAGE 2}
{$REGION PAGE2}
{BATTLES & MENUS}
{$REGION BATTLE AND MENUS}
      10710: //Start battle
      begin
         dummy := 'battle(';
         case boolean(FData[0]) of
            false: dummy := dummy + intToStr(FData[1])+ ', ';
            true: dummy := dummy + 'variable[' + intToStr(FData[1]) + '], '
         end;
         if FData[3] = 0 then
            dummy := dummy + 'false, '
         else
            dummy := dummy + 'true, ';
         //end if
         if FData[5] = 0 then
            dummy := dummy + 'false)'
         else
            dummy := dummy + 'true)';
         //end if
         if (FData[3] = 0) and (FData[4] = 0) then
            result := result + ansiString(dummy) + ';'
         else if (boolean(FData[4]) = true) or (FData[3] = 2) then
         begin
            result := result + 'case ' + ansiString(dummy) + ' of';
            inc(FCorrection);
            if FData[3] = 1 then
            begin
               result := result + LFCR + indent(FDepth) + 'br_escaped: Exit;'
            end
         end
         else if FData[3] = 1 then
         begin
            result := 'if ' + ansiString(dummy) + ' = br_escaped then' + LFCR + indent(FDepth + 1) + 'Exit;'
         end
         else raise EParseMessage.create('Unknown battle opcode configuration!');
         //end if
      end;
      20710: result := result + 'br_victory:' + LFCR + indent(FDepth) + 'begin';
      20711: result := result + 'br_escaped:' + LFCR + indent(FDepth) + 'begin';
      20712: result := result + 'br_defeated:' + LFCR + indent(FDepth) + 'begin';
      20713: result := result + 'end; //end of CASE block';

      10720: //call shop
      begin
         use(uses_menu);
         dummy := 'shop(' + intToStr(FData[0]) + ', ' + intToStr(FData[1]) + ', prepareStore(''';
         for I := 4 to high(FData) do
            dummy := dummy + intToStr(FData[i]) + ' ';
         dummy := dummy + '''))';
         if boolean(FData[2]) = true then
         begin
            assert(next.FOpcode = 20720);
            result := result + 'if ' + ansiString(dummy) + ' = true then'
         end
         else
            result := result + ansiString(dummy) + ';';
      end;
      20720: result := result + 'begin';
      20721: result := result + 'else begin';
      20722: ; //result := result + 'end;';

      10730: //inn
      begin
         use(uses_messages);
         dummy := 'inn(' + intToStr(FData[0]) + ', ' + intToStr(FData[1]) + ')';
         if boolean(FData[2]) = true then
         begin
            assert(next.FOpcode = 20730);
            result := result + 'if ' + ansiString(dummy) + ' = true then'
         end
         else
            result := result + ansiString(dummy) + ';';
      end;
      20730: result := result + 'begin';
      20731: result := result + 'else begin';
      20732: ; //result := result + 'end;';

      10740: //enter hero name
      begin
         use(uses_menu);
         result := result + 'hero[' + ansiString(intToStr(FData[0])) + '].name := inputText(';
         if FData[2] = 1 then
            result := result + 'hero[' + ansiString(intToStr(FData[0])) + '].name'
         else result := result + '''';
         result := result + ', ' + ansiString(intToStr(FData[0])) + ');'
      end;
{$ENDREGION}
{TELEPORTS AND EVENTS}
{$REGION TELEPORTS AND EVENTS}
      10810: //teleport
      begin
         use(uses_maps);
         result := result + 'teleport(' + ansiString(intToStr(FData[0])) + ', '
                   + ansiString(intToStr(FData[1])) + ',' + ansiString(intToStr(FData[2])) + ');';
      end;
      10820: //memorize location
      begin
         use(uses_maps);
         result := result + 'memorizeLocation(varD[' + ansiString(intToStr(FData[0]))
                   + '], varD[' + ansiString(intToStr(FData[1])) + '], varD[' + ansiString(intToStr(FData[2])) + ']);';
      end;
      10830: //go to memorized location
      begin
         use(uses_maps);
         result := result + 'teleport(variable[' + ansiString(intToStr(FData[0]))
                   + '], variable[' + ansiString(intToStr(FData[1])) + '], variable[' + ansiString(intToStr(FData[2])) + ']);';
      end;
      10840: //ride vehicle
      begin
         use(uses_maps);
         result := result + 'rideVehicle;';
      end;
      10850: //teleport vehicle
      begin
         use(uses_maps);
//result := result + 'teleportVehicle(' + eventDeref(FData[0] + 10002);
         result := result + 'teleportVehicle(' + eventDeref(FData[0] + 10002) + ', ';
         case boolean(FData[1]) of
            false: result := result + ansiString(intToStr(FData[2])) + ', ' + ansiString(intToStr(FData[3]))
                            + ', ' + ansiString(intToStr(FData[4])) + ');';
            true: result := result + 'variable[' + ansiString(intToStr(FData[2])) + '], variable[' + ansiString(intToStr(FData[3]))
                            + '], variable[' + ansiString(intToStr(FData[4])) + ']);';
         end;
      end;
      10860: //teleport event
      begin
         use(uses_maps);
         result := result + 'teleportEvent(' + eventDeref(FData[0]) + ', ';
         case boolean(FData[1]) of
            false: result := result + ansiString(intToStr(FData[2])) + ',' + ansiString(intToStr(FData[3])) + ')';
            true: result := result + 'variable[' + ansiString(intToStr(FData[2])) + '], variable[' + ansiString(intToStr(FData[3])) + ']);';
         end;
      end;
      10870: //swap events
      begin
         use(uses_maps);
         result := result + 'swapEvents(' + eventDeref(FData[0]) + ', ' + eventDeref(FData[1]) + ');';
      end;
      10910: //get terrain ID
      begin
         use(uses_maps);
         result := result + 'variable[' + ansiString(intToStr(FData[3])) + '] :=';
         result := result + 'getTerrainID(';
         case boolean(FData[0]) of
            false: result := result + ansiString(intToStr(FData[1])) + ', ' + ansiString(intToStr(FData[2])) + ');';
            true: result := result + 'variable[' + ansiString(intToStr(FData[1])) + '], variable[' + ansiString(intToStr(FData[2])) + ']);';
         end;
      end;
      10920: //get event ID
      begin
         use(uses_maps);
         result := result + 'variable[' + ansiString(intToStr(FData[3])) + '] := ';
         result := result + 'getEventID(';
         case boolean(FData[0]) of
            false: result := result + ansiString(intToStr(FData[1])) + ', ' + ansiString(intToStr(FData[2])) + ');';
            true: result := result + 'variable[' + ansiString(intToStr(FData[1])) + '], variable[' + ansiString(intToStr(FData[2])) + ']);';
         end;
      end;
{$ENDREGION}
{SCREEN ROUTINES}
{$REGION SCREEN ROUTINES}
      11010: //erase screen
      begin
         use(uses_maps);
         result := result + 'eraseScreen(' + C_TRANSITION_LIST[FData[0] + 1] + ');';
      end;
      11020: //show screen
      begin
         use(uses_maps);
         result := result + 'showScreen(' + C_TRANSITION_LIST[FData[0] + 1] + ');';
      end;
      11030: //set screen tone
      begin
         use(uses_maps);
         result := result + 'setScreenTone(';
         for I := 0 to 3 do
            result := result + ansiString(intToStr(fdata[i])) + ', ';
         result := result + ansiString(intToStr(FData[4] * 100)) + ', ';
         if FData[5] = 1 then
            result := result + 'true);'
         else result := result + 'false);'
      end;
      11040: //flash screen
      begin
         use(uses_maps);
         result := result + 'flashScreen(';
         for I := 0 to 3 do
            result := result + ansiString(intToStr(round(fdata[i] * (255 / 31)))) + ', ';
         result := result + ansiString(intToStr(FData[4] * 100)) + ', ';
         if FData[5] = 1 then
            result := result + 'true);'
         else result := result + 'false);'
      end;
      11050: //shake screen
      begin
         use(uses_maps);
         result := result + 'shakeScreen(' + ansiString(intToStr(FData[0])) + ', ' + ansiString(intToStr(FData[1])) + ', ';
         result := result + ansiString(intToStr(FData[2] * 100)) + ', ';
         if FData[3] = 1 then
            result := result + 'true);'
         else result := result + 'false);'
      end;
      11060: //pan screen
      begin
         use(uses_maps);
         case FData[0] of
            0: result := result + 'lockScreen;';
            1: result := result + 'unlockScreen;';
            2: result := result + 'panScreen(' + C_DIRECTIONS[FData[1]] + ', ' + ansiString(intToStr(FData[2])) + ', ';
            3: result := result + 'returnScreen(';
         end; //end of CASE block
         if FData[0] >= 2 then
         begin
            result := result + ansiString(intToStr(FData[3])) + ', ';
            if FData[4] = 1 then
               result := result + 'true);'
            else result := result + 'false);'
         end;
      end;
      11070: //weather
      begin
         use(uses_maps);
         result := result + 'setWeather(';
         result := result + C_WEATHER[FData[0]] + ', ' + ansiString(intToStr(FData[1] + 1)) + ');';
      end;
{$ENDREGION}
{IMAGE/SPRITE ROUTINES}
{$REGION IMAGE ROUTINES}
      11110: //show image
      begin
         use(uses_maps);
         j := 1;
         repeat
            i := inStringA(FName, '''', j);
            j := i + 2;
            if i <> -1 then
               strInsertAnsi(FName, '''', i);
         until i = -1;
         result := result + 'image[' + ansiString(intToStr(FData[0])) + '] := newImage(''' + FName + ''', ';
         case boolean(FData[1]) of
            true: result := result  + 'variable[' + ansiString(intToStr(FData[2])) + '], variable[' + ansiString(intToStr(FData[3])) + '], ';
            false: result := result  + ansiString(intToStr(FData[2])) + ', ' + ansiString(intToStr(FData[3])) + ', ';
         end;
         result := result + ansiString(intToStr(FData[5])) + ', ' + ansiString(intToStr(FData[6])) + ', ';
         if FData[4] = 1 then
            result := result + 'false, '
         else result := result + 'true, ';
         if FData[7] = 1 then
            result := result + 'true);'
         else result := result + 'false);';

         effect := false;
         for I := 8 to 11 do
            effect := effect or (FData[i] <> 100);
         if effect then
         begin
            result := result + LFCR + indent(FDepth) + 'image[' + ansiString(intToStr(FData[0])) + '].applyImageColors(';
            for I := 8 to 11 do
            begin
               result := result + ansiString(intToStr(fdata[i]));
               if i <> 11 then
                  result := result + ', '
            end;
            result := result + ');';
         end;

         if (FData[12] <> 0) then
         begin
            result := result + LFCR + indent(FDepth) + 'image[' + ansiString(intToStr(FData[0])) + '].applyImageEffect('
                             + C_IMAGE_EFFECTS[FData[12]] + ', ' + ansiString(intToStr(FData[13])) + ');';
         end;
      end;
      11120: //move image
      begin
         result := result + 'image[' + ansiString(intToStr(FData[0])) + '].timer := ' + ansiString(intToStr(FData[14] * 100)) + ';' + LFCR + indent(FDepth);
         result := result + 'image[' + ansiString(intToStr(FData[0])) + '].moveTo(';
         case boolean(FData[1]) of
            true: result := result  + 'variable[' + ansiString(intToStr(FData[2])) + '], variable[' + ansiString(intToStr(FData[3])) + '], ';
            false: result := result  + ansiString(intToStr(FData[2])) + ', ' + ansiString(intToStr(FData[3])) + ', ';
         end;
         assert(FData[4] = 0);
         assert(FData[7] = 0);
            result := result + ansiString(intToStr(FData[5])) + ', ' + ansiString(intToStr(FData[6])) + ');' + LFCR + indent(FDepth);
         result := result + 'image[' + ansiString(intToStr(FData[0])) + '].applyImageColors(';

         for I := 8 to 11 do
         begin
            result := result + ansiString(intToStr(fdata[i]));
            if i <> 11 then
              result := result + ', '
         end;
         result := result + ');';

         result := result + LFCR + indent(FDepth) + 'image[' + ansiString(intToStr(FData[0])) + '].applyImageEffect('
                                 + C_IMAGE_EFFECTS[FData[12]] + ', ' + ansiString(intToStr(FData[13])) + ');';

         if boolean(FData[15]) then
            result := result + LFCR + indent(FDepth) + 'wait(' + ansiString(intToStr(FData[14] * 100)) + ');';
         //end if
      end;
      11130: //erase image
         result := result + 'image[' + ansiString(intToStr(FData[0])) + '].erase;';
      11210: //show battle anim
      begin
         use(uses_maps);
         result := result + 'showBattleAnim(' + ansiString(intToStr(FData[0])) + ', '
                          + eventDeref(FData[1]) + ', ';
         case boolean(FData[2]) of
            false: result := result + 'false, ';
            true: result := result + 'true, ';
         end;
         case boolean(FData[3]) of
            false: result := result + 'false);';
            true: result := result + 'true);';
         end;
         //end if
      end;
      11310: //set hero translucency
      begin
         result := result + 'party.translucency := ';
         case boolean(FData[0]) of
            false: result := result + '3;';
            true: result := result + '0;';
         end;
      end;
      11320: //flash event
      begin
         use(uses_maps);
         result := result + eventDeref(FData[0]) + '.flash(';
         result := result + ansiString(intToStr(commons.round(FData[1] * MULTIPLIER_31))) + ', '
              + ansiString(intToStr(commons.round(FData[2] * MULTIPLIER_31))) + ', '
              + ansiString(intToStr(commons.round(FData[3] * MULTIPLIER_31))) + ', '
              + ansiString(intToStr(commons.round(FData[4] * MULTIPLIER_31))) + ', ' +
              ansiString(intToStr(FData[5] * 100)) + ', ';
         if boolean(FData[6]) then
            result := result + 'true);'
         else result := result + 'false);';
      end;
{$ENDREGION}
{EVENT MOVEMENT}
{$REGION EVENT MOVEMENT}
      11330: //move event
      begin
         use(uses_maps);
         result := result + eventDeref(FData[0]) + '.move(' + ansiString(intToStr(FData[1])) +  ', ';
         if boolean(FData[3]) then
            result := result + 'true, '
         else result := result + 'false, ';
         result := result + 'prepareRoute(''';
         //Handle move event itself
         for I := 4 to high(FData) do
            result := result + ansiString(intToStr(FData[i])) + ' ';
         result := result + ''', ';
         if boolean(FData[2]) then
            result := result + 'true));'
         else result := result + 'false));';
      end;
      11340: //wait until moved
      begin
         use(uses_maps);
         result := result + 'waitUntilMoved;';
      end;
      11350: //stop all movement
      begin
         use(uses_maps);
         result := result + 'stopMoveScripts;';
      end;
      11410: //wait
         result := result + 'wait(' + ansiString(intToStr(FData[0] * 100)) + ');';
{$ENDREGION}
{$ENDREGION}
{SOUNDS AND MUSIC}
{$REGION SOUNDS AND MUSIC}
      11510: //play BGM
      begin
         use(uses_system);
         j := 1;
         repeat
            i := inStringA(FName, '''', j);
            j := i + 2;
            if i <> -1 then
               strInsertAnsi(FName, '''', i);
         until i = -1;
         result := result + 'playMusic(''' + FName + ''', ';
         for I := 0 to 3 do
         begin
            result := result + ansiString(intToStr(FData[i]));
            if i < 3 then
               result := result + ', '
            else result := result + ');';
         end;
      end;
      11520: //BGM fade out
      begin
         use(uses_system);
         result := result + 'fadeOutMusic(' + ansiString(intToStr(FData[0])) + ');';
      end;
      11530: //memorize BGM
      begin
         use(uses_system);
         result := result + 'memorizeBgm;';
      end;
      11540: //play memorized BGM
      begin
         use(uses_system);
         result := result + 'playMemorizedBgm;';
      end;
      11550: //Play sound
      begin
         use(uses_system);
         j := 1;
         repeat
            i := inStringA(FName, '''', j);
            j := i + 2;
            if i <> -1 then
               strInsertAnsi(FName, '''', i);
         until i = -1;
         result := result + 'playSound(''' + FName + ''', ';
         for I := 0 to 2 do
         begin
            result := result + ansiString(intToStr(FData[i]));
            if i < 2 then
               result := result + ', '
            else result := result + ');';
         end;
      end;
{      11560: //play movie:
      begin
         result := result + 'Play Movie: ' + FName + ' Position: (';
         case boolean(FData[0]) of
            false: result := result + intToStr(FData[1]) + ', ' + intToStr(FData[2]) + ')';
            true: result := result + 'V[' + intToStr(FData[1]) + '], V[' + intToStr(FData[2]) + '])';
         end;
         result := result + ', Size:(' + intToStr(FData[3]) + 'x' + intToStr(FData[4]) + ')';
      end;}
{$ENDREGION}
{MAP MANAGEMENT}
{$REGION MAP MANAGEMENT}
      11610: //key input
      begin
         use(uses_system);
         result := result + 'variable[' + ansiString(intToStr(FData[0])) + '] := keyScan(';
         //key codes
         i := 0;
         if boolean(FData[2]) and boolean(FData[3]) and boolean(FData[4]) then
            result := result + 'KS_ALL'
         else begin
            if boolean(FData[2]) then
            begin
               inc(i);
               result := result + 'KS_DIRS';
            end;
            if boolean(FData[3]) then
            begin
               if i > 0 then
                  result := result + ' or ';
               inc(i);
               result := result + 'KS_ACTION';
            end;
            if boolean(FData[3]) then
            begin
               if i > 0 then
                  result := result + ' or ';
               inc(i);
               result := result + 'KS_CANCEL';
            end;
            if i = 0 then
               result := result + '0';
         end;
         result := result + ', ';
         result := result + ansiString(BoolToStr(boolean(FData[1]), true)) + ');'
         //end if
      end;
      {11710: //change chipset
         result := result + 'Change Current Chipset: ' + chipsetName(FData[0]);}
      11720: //change BG
      begin
         use(uses_maps);
         result := result + 'setBgImage(''' + FName + ''', ';
         if FData[0] = 0 then
            result := result + '0, '
         else result := result + ansiString(intToStr(FData[3])) + ', ';
         if FData[1] = 0 then
            result := result + '0, '
         else result := result + ansiString(intToStr(FData[5])) + ', ';
         if FData[0] = 0 then
            result := result + 'false, '
         else result := result + ansiString(BoolToStr(boolean(FData[2]), true)) + ', ';
         if FData[1] = 0 then
            result := result + 'false);'
         else result := result + ansiString(BoolToStr(boolean(FData[4]), true)) + ');';
      end;
{      11740: //encounter rate
         result := result + 'Set Encounter Rate: ' + intToStr(FData[0]) + ' Steps';
      11750: //change chip
      begin
         result := result + 'Change Chip: ';
         case boolean(FData[0]) of
            false: result := result + 'Lower Chip ';
            true: result := result + 'Upper Chip ';
         end;
         result := result + intToStr(FData[1] + 1) + ' to ' + intToStr(FData[2] + 1);
      end;
      11810: //set teleport place
      begin
         result := result + 'Teleport Position: Map' + intToStr(FData[1]) + ', ';
         case boolean(FData[0]) of
            false: result := result + 'Set to: (' + intToStr(FData[2]) + ',' + intToStr(FData[3]) + ')';
            true: result := result + 'Disable';
         end;
      end;
      11820: //enable teleport
         case boolean(FData[0]) of
            false: result := result + 'Disable Teleport on Current Map';
            true: result := result + 'Enable Teleport on Current Map';
         end;
      11830: //set escape place
      begin
         result := result + 'Escape Position: Map' + intToStr(FData[0]) + ', ';
         result := result + 'Set to: (' + intToStr(FData[1]) + ',' + intToStr(FData[2]) + ')';
      end;
      11840: //enable Escape
         case boolean(FData[0]) of
            false: result := result + 'Disable Escape on Current Map';
            true: result := result + 'Enable Escape on Current Map';
         end;
      11910: //save
         result := result + 'Call Save Menu';
      11930: //save menu enable
      begin
         case boolean(FData[0]) of
            false: result := result + 'Disable ';
            true: result := result + 'Enable ';
         end;
         result := result + 'Saving for this map';
      end;
}
{$ENDREGION}
{More internal logic}
{$REGION MORELOGIC}
      11950: //call system menu
      begin
         use(uses_menu);
         result := result + 'openMenu';
      end;
      11960: //system menu
      begin
         result := result + 'menuEnabled := ';
         case boolean(FData[0]) of
            false: result := result + 'false;';
            true: result := result + 'true;';
         end;
      end;
      12010: //IF
      begin
         result := result + 'if ';
         case FData[0] of
            0:
            begin
               result := result + 'switch[' + ansiString(intToStr(fdata[1])) + '] = ';
               case boolean(FData[2]) of
                  false: result := result + 'true';
                  true: result := result + 'false';
               end;
            end;
            1:
            begin
               result := result + 'variable[' + ansiString(intToStr(FData[1])) + '] ';
               case fdata[4] of
                  0: result := result + '= ';
                  1: result := result + '>= ';
                  2: result := result + '<= ';
                  3: result := result + '> ';
                  4: result := result + '< ';
                  5: result := result + '<> '
                  else raise EParseMessage.create('Unknown comparison operator x' + intTohex(FData[4], 2) + ' under opcode 12010!');
               end;
               case boolean(FData[2]) of
                  false: result := result + ansiString(intToStr(FData[3]));
                  true: result := result + 'variable[' + ansiString(intToStr(FData[3])) + ']';
               end;
            end;
            2, 10:
            begin
               if FData[0] = 2 then
                  result := result + 'timer.time '
               else result := result + 'timer2.time ';
               case boolean(FData[2]) of
                  true: result := result + '<= ';
                  false: result := result + '>= ';
               end;
               result := result + ansiString(intToStr(FData[1]));
            end;
            3:
            begin
               result := result + 'party.money ';
               case boolean(FData[2]) of
                  true: result := result + '<= ';
                  false: result := result + '>= ';
               end;
               result := result + ansiString(intToStr(FData[1]));
            end;
            4:
            begin
               result := result + 'party.inventory.contains(';
               result := result + ansiString(intToStr(FData[1])) + ') = ';
               case boolean(FData[2]) of
                  true: result := result + 'false';
                  false: result := result + 'true';
               end;
            end;
            5:
            begin
               result := result + 'hero[' + ansiString(intToStr(FData[1])) + '].';
               case FData[2] of
                  0: result := result + 'inParty = true';
                  1: result := result + 'name = ''' + FName + '''';
                  2: result := result + 'level >= ' + ansiString(intToStr(FData[3]));
                  3: result := result + 'hp >= ' + ansiString(intToStr(FData[3]));
                  4: result := result + 'skill[' + ansiString(intToStr(FData[3])) + '] = true';
                  5: result := result + 'equipped(' + ansiString(intToStr(FData[3])) + ') = true';
                  6: result := result + 'condition[' + ansiString(intToStr(FData[3])) + '] = true';
               end;
            end;
            6:
            begin
               result := result + eventDeref(FData[1]) + '.facing = ';
               case FData[2] of
                  0: result := result + '8';
                  1: result := result + '6';
                  2: result := result + '2';
                  3: result := result + '4';
               end;
            end;
            7: result := result + eventDeref(FData[1] + 10002) + '.inUse = true';
            8: result := result + 'buttonStart = true';
            9: result := result + 'bgm.looped = true';
            else assert(false);
         end;
         result := result + ' then' + LFCR + indent(FDepth) + 'begin';
      end;
      22010: result := result + 'else begin';
      22011: ; //result := result + 'end;';

      12110: //label
      begin
         if parent.FLabelList <> '' then
            parent.FLabelList := parent.FLabelList + ', ';
         parent.FLabelList := parent.FLabelList + 'L' + ansiString(intToStr(FData[0]));
         result := result + 'L' + ansiString(intToStr(FData[0])) + ':';
      end;
      12120: //the dreaded GOTO
         result := result + 'goto L' + ansiString(intToStr(FData[0])) + ';';
      12210: //loop
      begin
         result := result + 'while true do' + LFCR + indent(FDepth) + 'begin';
         inc(FParent.FLoopDepth);
      end;
      22210:
         dec(FParent.FLoopDepth); //result := result + ':End Loop';
      12220: //Break
      begin
         if FParent.FLoopDepth > 0 then
            result := result + 'break;'
         else result := result + 'exit;';
      end;
      12310: //stop parallel events
         result := result + 'exit;';
{$ENDREGION}
{EVENT MANAGEMENT}
{$REGION EVENT MANAGEMENT}
      12320: //delete event
      begin
         use(uses_system);
         result := result + 'deleteCharacter(false);';
      end;
      12330: //call event
      begin
         use(uses_system);
         case FData[0] of
            0: result := result + 'callGlobalEvent(' + ansiString(intToStr(FData[1])) + ');';
            1:
            begin
               if FData[1] <> 10005 then
                  result := result + 'callEvent(' + ansiString(intToStr(FData[1])) + ', ' + ansiString(intToStr(FData[2])) + ');'
               else
                  result := result + 'callEvent(thisEvent.id, ' + ansiString(intToStr(FData[2])) + ');';
            end;
            2: result := result + 'callEvent(variable[' + ansiString(intToStr(FData[1])) + '], variable[' + ansiString(intToStr(FData[2])) + ']);';
         end;
      end;
{$ENDREGION}
{UNTRANSLATED}
{$REGION UNTRANSLATED}
      12410, 22410: //note
         result := result + '//' + FName;
{      12420: // game over
         result := result + 'Game Over';
      12510: //back to title
         result := result + 'Go to Title Screen';
      else
      begin
         msgBox('Unrecognized opcode: ' + intToStr(FOpcode), 'Event parser message');
         result := result + intToStr(FOpcode) + ' ';
         result := result + intToStr(FDepth) + ' ';
         result := result + intToStr(length(FName)) + ' ';
         if length(FName) > 0 then
            result := result + FName + ' ';
         result := result + intToStr(length(FData)) + ' ';
         if length(FData) > 0 then
            for I := 0 to high(FData) do
               result := result + intToHex(FData[i], 2) + ' ';
            //end FOR
         //end IF
      end;
      //end CASE}
{$endregion}
   end;
   if (not continuing) and (FOpcode <> 0) then
      result := result + LFCR;
end;

{IfDef'd functions}
{$REGION GETTEXT}
{$IFDEF EDITOR}
function TEventCommand.heroName(const id: word): ansiString;
begin
   result := GDatabase.hero[id].name;
end;

function TEventCommand.itemName(const id: word): ansiString;
begin
   result := GDatabase.item[id].name;
end;

function TEventCommand.mpartyName(const id: word): ansiString;
begin
   result := GDatabase.getMonsterParty(id).name;
end;

function TEventCommand.skillName(const id: word): ansiString;
begin
   result := GDatabase.skill[id].name;
end;

function TEventCommand.animName(const id: word): ansiString;
begin
   result := GDatabase.anim[id].name;
end;

function TEventCommand.conditionName(const id: word): ansiString;
begin
   result := GDatabase.condition[id].name;
end;

function TEventCommand.chipsetName(const id: word): ansiString;
begin
   result := GDatabase.getChipset(id).name;
end;
{$ENDIF}
{$ENDREGION}

{ TCaseStackFrame }

constructor TCaseStackFrame.Create(const depth, count: byte; const elseCase: boolean);
begin
   FDepth := depth;
   FCount := count;
   FElse := elseCase;
   FCurrent := 0;
end;

{ Classless }
{$REGION Classless}
function decodeMove(const data: array of integer; var pos: integer): string;
var i: byte;
begin
   case data[pos] of
      0: result := 'Up';
      1: result := 'Right';
      2: result := 'Down';
      3: result := 'Left';
      4: result := 'Right/Up';
      5: result := 'Right/Down';
      6: result := 'Left/Down';
      7: result := 'Left/Up';
      8: result := 'Random Walk';
      9: result := 'Walk Towards Hero';
      $A: result := 'Walk Away From Hero';
      $B: result := 'Walk Forward';
      $C: result := 'Face Up';
      $D: result := 'Face Right';
      $E: result := 'Face Down';
      $F: result := 'Face Left';
      $10: result := 'Turn Right';
      $11: result := 'Turn Left';
      $12: result := 'Turn 180';
      $13: result := 'Turn 90 (Random)';
      $14: result := 'Face Random Direction';
      $15: result := 'Face Hero';
      $16: result := 'Face Away From Hero';
      $17: result := 'Wait 1';
      $18: result := 'Start Jump';
      $19: result := 'End Jump';
      $1A: result := 'Fix Direction';
      $1B: result := 'Cancel Fix Dir.';
      $1C: result := 'Move Speed Up';
      $1D: result := 'Move Speed Down';
      $1E: result := 'Move Freq. Up';
      $1F: result := 'Move Freq. Down';
      $20:
      begin
         result := 'Switch ' + intToStr(data[pos+1]) + ' ON';
         inc(pos);
      end;
      $21:
      begin
         result := 'Switch ' + intToStr(data[pos+1]) + ' OFF';
         inc(pos);
      end;
      $22:
      begin
         result := 'Change Sprite';
         inc(pos, data[pos + 1] + 2); //skip data
      end;
      $23:
      begin
         result := 'Play SFX';
         inc(pos, data[pos+1]); //skip data
         i := 0;
         repeat
            inc(pos);
            if data[pos] < 129 then
               inc(i);
         until i >= 4;
      end;
      $24: result := 'Clipping Off';
      $25: result := 'Clipping On';
      $26: result := 'Stop Animation';
      $27: result := 'Resume Animation';
      $28: result := 'Transparency Up 1';
      $29: result := 'Transparency Down 1';
      else
         raise EParseMessage.create('Unknown movement opcode x' + intToHex(data[pos], 2) + ' found!');
      //end else
   end; //end case
end;

procedure fillInEventInt(const expected: byte; out theResult: integer);
begin
   case expected of
      2,3: theResult := 0;
   else
      begin
         msgBox ('No case implemented for x' + IntToHex(expected, 2) + '!', 'fillInEventInt says:', MB_OK);
         raise EMessageAbort.Create
      end
   end;
end;

procedure fillInEPageInt(const expected: byte; out theResult: integer);
begin
   case expected of
      0, $16, $17, $19, $1F, $21, $23, $24, $33: theResult := 0;
      $18: theResult := 1;
      $20: theResult := 3;
      $25: theResult := 2;
   else
      begin
         msgBox ('No case implemented for x' + IntToHex(expected, 2) + '!', 'fillInEPageInt says:', MB_OK);
         raise EMessageAbort.Create
      end
   end;
end;

procedure fillInEConInt(const expected: byte; out theResult: integer);
begin
   case expected of
      2..4, 6, 7, $A: theResult := 1;
      0, 5, 8, 9: theResult := 0;
   else
      begin
         msgBox ('No case implemented for x' + IntToHex(expected, 2) + '!', 'fillInEConInt says:', MB_OK);
         raise EMessageAbort.Create
      end
   end;
end;

procedure fillInGlobalEConInt(const expected: byte; out theResult: integer);
begin
   case expected of
      $D: theResult := 1;
   else
      begin
         msgBox ('No case implemented for x' + IntToHex(expected, 2) + '!', 'fillInGlobalEConInt says:', MB_OK);
         raise EMessageAbort.Create
      end
   end;
end;

procedure fillInEMoveInt(const expected: byte; out theResult: integer);
begin
   case expected of
      $15: theResult := 1;
   else
      begin
         msgBox ('No case implemented for x' + IntToHex(expected, 2) + '!', 'fillInEMoveInt says:', MB_OK);
         raise EMessageAbort.Create
      end
   end;
end;
{$ENDREGION}
end.
