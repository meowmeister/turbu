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

uses math, types, classes,  //system libs
     charset_data, move_data, //turbu libs
     turbu_containers, turbu_defs;

type
   TMoveType = (still, randomMove, cycle_ud, cycle_lr, chase_hero, flee_hero, by_route);
   TStartCondition = (by_key, by_touch, by_collision, automatic, parallel, on_call);
   TAnimType = (at_sentry, at_jogger, at_fixedDir, at_fixedJog, at_statue, at_spinRight);
   TPageConditionSet = (switch1, switch2, variable1, item, hero, timer, timer2);
   TBattlePageConditionSet = (bp_switch1, bp_switch2, bp_variable1, bp_turns,
     bp_monsterTime, bp_heroTime, bp_exhaustion, bp_monsterHP, bp_HeroHP, bp_commandUsed);

   TPageConditions = array[TPageConditionSet] of boolean;
   TBattlePageConditions = set of TBattlePageConditionSet;

type
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
   public
      constructor create(input: TStringStream);
      constructor createGlobal(input: TStream);

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

   TBattleEventConditions = class(TObject)
   private
      FConditions: TBattlePageConditions;
      FSwitch1: word;
      FSwitch2: word;
      FVariable: word;
      FVarValue: integer;
      FTurnsMultiple: integer;
      FTurnsConst: integer;
      FExhaustionMin: word;
      FExhaustionMax: word;
      FMonsterHP: word;
      FMonsterHPMin: integer;
      FMonsterHPMax: integer;
      FHeroHP: word;
      FHeroHPMin: integer;
      FHeroHPMax: integer;
      FMonsterTurn: word;
      FMonsterTurnsMultiple: integer;
      FMonsterTurnsConst: integer;
      FHeroTurn: word;
      FHeroTurnsMultiple: integer;
      FHeroTurnsConst: integer;
      FHeroCommandWho: integer;
      FHeroCommandWhich: integer;
   public
      constructor create(input: TStream);

      property conditions: TBattlePageConditions read FConditions;
      property switch1Set: word read FSwitch1;
      property switch2Set: word read FSwitch2;
      property variableSet: word read FVariable;
      property variableValue: integer read FVarValue;
      property turnsConst: integer read FTurnsConst;
      property turnsMultiple: integer read FTurnsMultiple;
      property monsterTurn: word read FMonsterTurn;
      property monsterTurnsConst: integer read FMonsterTurnsConst;
      property monsterTurnsMultiple: integer read FMonsterTurnsMultiple;
      property heroTurn: word read FHeroTurn;
      property heroTurnsConst: integer read FHeroTurnsConst;
      property heroTurnsMultiple: integer read FHeroTurnsMultiple;
      property exhaustionMin: word read FExhaustionMin;
      property exhaustionMax: word read FExhaustionMax;
      property monsterHP: word read FMonsterHP;
      property monsterHPMin: integer read FMonsterHPMin;
      property monsterHPMax: integer read FMonsterHPMax;
      property heroHP: word read FHeroHP;
      property heroHPMin: integer read FHeroHPMin;
      property heroHPMax: integer read FHeroHPMax;
      property heroCommandWho: integer read FHeroCommandWho;
      property heroCommandWhich: integer read FHeroCommandWhich;
   end;

   TEvent = class;
   TEventBlock = class;
   TEventPage = class;

   TEventCommand = class(TObject)
   private
      FOpcode: integer;
      FDepth: byte;
      FName: utf8String;
      FData: TArray<integer>;
      function getScript: ansiString;
   public
      constructor Create(input: TStream); overload;
      constructor Create(opcode, value: integer); overload;
      property script: ansiString read getScript;
      property opcode: integer read FOpcode;
      property name: utf8String read FName;
      property data: TArray<integer> read FData;
      property indent: byte read FDepth;
   end;

   TEventCommandList = class(TRpgObjectList<TEventCommand>);

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

   TAnimFrame = (left, center, right);

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
      FCommands: TEventCommandList;
      FParent: TEvent;
      FLoopDepth: byte;
      FIndexList: array[1..10] of boolean;
      FLabelList: ansiString;
      FOverride: boolean;
      FGlobal: boolean;

      function hasScriptFunction: boolean; inline;

      procedure stripLastSem(var data: ansiString);
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
      property parent: TEvent read FParent;
      property hasScript: boolean read hasScriptFunction;
      property opcode: TEventCommandList read FCommands;
      property len: word read getLength;
      property commands: TEventCommandList read FCommands;
   end;

   TBattleEventPage = class
   private
      FConditions: TBattleEventConditions;
      FCommands: TEventCommandList;
      FBase: AnsiString;
   public
      constructor Create(input: TStream; const id: integer);
      destructor Destroy; override;

      property Conditions: TBattleEventConditions read FConditions;
      property Commands: TEventCommandList read FCommands;
      property Base: AnsiString read FBase write FBase;
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
      function getpage(x: word): TEventPage; inline;
      procedure setPage(x: word; input:TEventPage); inline;
      procedure setCurrentlyPlaying(value: boolean); inline;
   public
      constructor createGlobal(input: TStream; expected: word; parent: TEventBlock);
      constructor create(input: TStringStream; parent: TEventBlock);
      destructor Destroy; override;
      property page[x: word]: TEventPage read getPage write setPage; default;
      property location: TPoint read FLocation write FLocation;
      property id: word read FID write FID;
      property name: ansiString read FName write FName;
      property len: smallint read FLength write FLength;
      property parent: TEventBlock read FParent;
      property lastCurrentPage: TEventPage read FCurrentPage;
      property updated: boolean read FPageChanged;
      property locked: boolean read FLocked write FLocked;
      property deleted: boolean read FDeleted write FDeleted;
   end;

   TEventBlock = class(TObject)
   private
      FLength: word;
      FEvents: array of TEvent;
      function getEvent(x: word): TEvent;
      {$IFDEF EDITOR}
      procedure setEvent(x: word; input: TEvent);
      {$ENDIF}
   public
      constructor create(const eventData: ansiString); overload;
      constructor create(const input: TStream); overload;
      destructor Destroy; override;
      function find(const id: word): TEvent;

      property events[x: word]: TEvent read getEvent {$IFDEF EDITOR}write setEvent{$ENDIF}; default;
      property len: word read FLength;
   end;

implementation

uses sysUtils, windows, //system libs
     fileIO, BER, commons, formats;

{const
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

   END_BLOCK: array[1..2] of integer = (20713, 20141); }

procedure fillInEventInt(const expected: byte; out theResult: integer); forward;
procedure fillInEPageInt(const expected: byte; out theResult: integer); forward;
procedure fillInEConInt(const expected: byte; out theResult: integer); forward;
procedure fillInBEConInt(const expected: byte; out theResult: integer); forward;
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
   FLength := 1;
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
   FCommands := TEventCommandList.Create(true);

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
         FCommands.Add(TEventCommand.create(opStream));
      until opStream.Position >= opStream.Size - 1;
   finally
      opStream.free;
   end;

//ends with a 00?
   assert(peekAhead(input, 0));
   FEventText := '';
   FScriptText := '';
   FLabelList := '';
end;

constructor TEventPage.createGlobal(input: TStream; parent: TEvent);
var
   dummy: integer;
   opStream: TStringStream;
begin
   inherited create;
   FParent := parent;
   FCommands := TEventCommandList.Create(true);
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
         FCommands.Add(TEventCommand.create(opStream));
      until opStream.Position >= opStream.Size - 1;
   finally
      opStream.free;
   end;
//ends with a 00?
   assert(peekAhead(input, 0));
end;

constructor TEventPage.create(input: ansiString; const expected: word; parent: TEvent);
begin
   inherited create;
   FParent := parent;
   FCommands := TEventCommandList.Create(true);
   FID := expected;
   FConditions := TEventConditions.create(nil);
   FEventText := '';
   FScriptText := input;
   FLabelList := '';
end;

destructor TEventPage.Destroy;
var
   i: integer;
begin
   FConditions.Free;
   FMoveScript.Free;
   FCommands.free;
   inherited;
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

{ TBattleEventConditions }

constructor TBattleEventConditions.create(input: TStream);
begin
   inherited Create;
   if GProjectFormat = pf_2k then
      word(FConditions) := getNumSec(1, input, fillInZeroInt)
   else if peekAhead(input, 1) then
   begin
      assert(peekAhead(input, 2));
      input.Read(FConditions, 2);
   end;
   FSwitch1 := getNumSec(2, input, fillInBEConInt);
   FSwitch2 := getNumSec(3, input, fillInBEConInt);
   FVariable := getNumSec(4, input, fillInBEConInt);
   FVarValue := getNumSec(5, input, FillInZeroInt);
   FTurnsMultiple := getNumSec(6, input, FillInZeroInt);
   FTurnsConst := getNumSec(7, input, FillInZeroInt);
   FExhaustionMin := getNumSec(8, input, FillInZeroInt);
   FExhaustionMax := getNumSec(9, input, FillInZeroInt);
   FMonsterHP := getNumSec($a, input, fillInZeroInt) + 1;
   FMonsterHPMin := getNumSec($b, input, FillInZeroInt);
   FMonsterHPMax := getNumSec($c, input, FillInZeroInt);
   FHeroHP := getNumSec($d, input, fillInBEConInt);
   FHeroHPMin := getNumSec($e, input, FillInZeroInt);
   FHeroHPMax := getNumSec($f, input, FillInZeroInt);
   FMonsterTurn := getNumSec($10, input, FillInZeroInt) + 1;
   FMonsterTurnsMultiple := getNumSec($11, input, FillInZeroInt);
   FMonsterTurnsConst := getNumSec($12, input, FillInZeroInt);
   FHeroTurn := getNumSec($13, input, fillInBEConInt);
   FHeroTurnsMultiple := getNumSec($14, input, FillInZeroInt);
   FHeroTurnsConst := getNumSec($15, input, FillInZeroInt);
   FHeroCommandWho := getNumSec($16, input, fillInBEConInt);
   FHeroCommandWhich := getNumSec($17, input, FillInZeroInt);
   if not (peekAhead(input, 0)) then
      raise EParseMessage.CreateFmt('Unknown section %d found at end of TEventConditions', [peekAhead(input)]);
end;

{ TEventCommand }

constructor TEventCommand.create(input: TStream);
var
   converter: intx80;
   i: Integer;
begin
   converter := TBerConverter.Create(input);
   FOpcode := converter.getData;
   FDepth := getNext(input);
   if not peekAhead(input, 0) then
      FName := utf8String(getString(input))
   else
      setLength(FName, 0);
   converter.read(input);
   setLength(FData, converter.getData);
   if length(FData) > 0 then
      for i := 1 to length(FData) do
      begin
         converter.read(input);
         FData[i-1] := converter.getData;
      end;
end;

constructor TEventCommand.Create(opcode, value: integer);
begin
   FOpcode := opcode;
   SetLength(FData, 1);
   FData[0] := value;
end;

function TEventCommand.getScript: ansiString;
begin
   result := '';
end;


{ Classless }

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
      $25: theResult := 3;
   else
      begin
         msgBox ('No case implemented for x' + IntToHex(expected, 2) + '!', 'fillInEPageInt says:', MB_OK);
         raise EMessageAbort.Create
      end
   end;
end;

procedure fillInBEConInt(const expected: byte; out theResult: integer);
begin
   if expected in [2..4, $D, $13, $16] then
      theResult := 1
   else begin
      msgBox ('No case implemented for x' + IntToHex(expected, 2) + '!', 'fillInEConInt says:', MB_OK);
      raise EMessageAbort.Create
   end
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

{ TBattleEventPage }

constructor TBattleEventPage.Create(input: TStream; const id: integer);
var
   converter: intX80;
   substream: TStringStream;
   size: integer;
begin
   inherited Create;
   assert(converter.Create(input).getData = id);
   substream := TStringStream.Create(getStrSec(2, input, nil));
   try
      FConditions := TBattleEventConditions.create(substream);
   finally
      substream.Free;
   end;
   size := getNumSec($b, input, nil);
   FBase := getStrSec($c, input, nil);
   substream := TStringStream.Create(FBase);
   try
      assert(substream.Size = size);
      FCommands := TEventCommandList.Create;
      while substream.Position < substream.Size do
         FCommands.Add(TEventCommand.Create(substream));
//      assert(peekAhead(substream, 0));
   finally
      substream.Free;
   end;
   assert(peekAhead(input, 0));
end;

destructor TBattleEventPage.Destroy;
begin
   FCommands.Free;
   FConditions.Free;
   inherited Destroy;
end;

end.
