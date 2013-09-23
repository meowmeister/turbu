unit monster;
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

interface
uses
   Types, Classes,
   events;

type
   TMonster = class
   public
      name: ansiString;
      filename: ansiString;
      transparent: boolean;
      flying: boolean;
      colorShift: integer;
      stats: array[1..6] of integer;
      exp: integer;
      money: integer;
      item: integer;
      itemChance: integer;
      canCrit: boolean;
      critChance: integer;
      oftenMiss: boolean;
      conditionModifiers: array of byte;
      dtypeModifiers: array of byte;
      actions: ansiString;
      constructor Create(theLDB: TStream; const id: word);
   end;

   TMonsterElement = record
      id: integer;
      position: TPoint;
      invisible: boolean;
      constructor Create(theLDB: TStream; i: integer);
   end;

   TMonsterParty = class
   public
      name: ansiString;
      monsters: array of TMonsterElement;
      autoAlign: boolean;
      rTag: boolean;
      habitats: array of boolean;
      events: array of TBattleEventPage;
      constructor Create(theLDB: TStream; const id: word);
      destructor Destroy; override;
   end;

implementation
uses
   Windows, SysUtils,
   BER, Commons, FileIO;

procedure fillInMonsterInt(const expected: byte; out theResult: integer); forward;
procedure fillInMonsterElementInt(const expected: byte; out theResult: integer); forward;

{ TMonster }

constructor TMonster.Create(theLDB: TStream; const id: word);
var
   converter: intx80;
   i: byte;
begin
   inherited Create;
   try
      converter := TBerConverter.Create(theLDB);
      if converter.getData <> id then
         raise EParseMessage.CreateFmt('Monster section %d of RPG_RT.LDB not found!', [id]);
      name := getStrSec(1, theLDB, fillInBlankStr);
      filename := getStrSec(2, theLDB, fillInBlankStr);
      colorShift := getNumSec(3, theLDB, fillInZeroInt);
      for i := 1 to 6 do
         stats[i] := getNumSec(3 + i, theLDB, fillInMonsterInt);
      self.transparent := getChboxSec($a, theLDB, fillInZeroInt);
      exp := getNumSec($b, theLDB, fillInZeroInt);
      money := getNumSec($c, theLDB, fillInZeroInt);
      item := getNumSec($d, theLDB, fillInZeroInt);
      itemChance := getNumSec($e, theLDB, fillInMonsterInt);
      canCrit := getChboxSec($15, theLDB, fillInZeroInt);
      critChance := getNumSec($16, theLDB, fillInMonsterInt);
      oftenMiss := getChboxSec($1A, theLDB, fillInZeroInt);
      flying := getChboxSec($1c, theLDB, fillinZeroInt);
      setLength(conditionModifiers, getNumSec($1F, theLDB, fillInZeroInt) + 1);
      if length(conditionModifiers) > 1 then
         getArraySec($20, theLDB, conditionModifiers[1])
      else skipSec($20, theLDB);
      setLength(dtypeModifiers, getNumSec($21, theLDB, fillInZeroInt) + 1);
      if length(dtypeModifiers) > 1 then
         getArraySec($22, theLDB, dtypeModifiers[1])
      else skipSec($22, theLDB);
      actions := getStrSec($2a, theLDB, fillInBlankStr);
      if not peekAhead(theLDB, 0) then
         raise EParseMessage.createFmt('Monster section %d final 0 not found', [id]);
   except
      on E: EParseMessage do
      begin
         msgBox(E.message, 'TMonster.Create says:', MB_OK);
         raise EMessageAbort.Create;
      end;
   end; // end of TRY block
end;

{ TMonsterElement }

constructor TMonsterElement.Create(theLDB: TStream; i: integer);
begin
   assert(TBerConverter.Create(theLDB).getData = i);
   id := getNumSec(1, theLDB, fillInMonsterElementInt);
   assert(id > 0);
   position.X := getNumSec(2, theLDB, fillInZeroInt);
   position.Y := getNumSec(3, theLDB, fillInZeroInt);
   invisible := getChboxSec(4, theLDB, fillInZeroInt);
   if not peekAhead(theLDB, 0) then
      raise EParseMessage.create('Monster element final 0 not found');
end;

{ TMonsterParty }

constructor TMonsterParty.Create(theLDB: TStream; const id: word);
var
   converter: intx80;
   i: integer;
   eventStream: TStringStream;
begin
   inherited Create;
   try
      converter := TBerConverter.Create(theLDB);
      if converter.getData <> id then
         raise EParseMessage.CreateFmt('Monster Party section %d of RPG_RT.LDB not found!', [id]);
      name := getStrSec(1, theLDB, fillInBlankStr);
      assert(peekAhead(theLDB, 2));
      converter.read(theLDB); //skip length section
      converter.read(theLDB);
      setLength(monsters, converter.getData + 1);
      for I := 1 to high(monsters) do
         monsters[i] := TMonsterElement.Create(theLDB, i);
      autoAlign := getChboxSec(3, theLDB, fillInZeroInt);
      setLength(habitats, getNumSec(4, theLDB, fillInZeroInt));
      if length(habitats) = 0 then
         skipSec(5, theLDB)
      else getArraySec(5, theLDB, habitats[0]);
      rTag := getChboxSec(6, theLDB, FillInZeroInt);
      eventStream := TStringStream.Create(getStrSec($b, theLDB, nil));
      try
         setLength(events, converter.Create(eventStream).getData + 1);
         for i := 1 to high(events) do
            events[i] := TBattleEventPage.Create(eventStream, i);
      finally
         eventStream.Free;
      end;
      if not peekAhead(theLDB, 0) then
         raise EParseMessage.createFmt('Monster party section %d final 0 not found', [id]);
   except
      on E: EParseMessage do
      begin
         msgBox(E.message, 'TMonsterParty.Create says:', MB_OK);
         raise EMessageAbort.Create;
      end;
   end;
end;

destructor TMonsterParty.Destroy;
var
   i: integer;
begin
   for I := 1 to high(events) do
      events[i].Free;
   inherited;
end;

procedure fillInMonsterInt(const expected: byte; out theResult: integer);
begin
   case expected of
      4..9: theResult := 10;
      $e: theResult := 100;
      $16: theResult := 30;
   else
      begin
         msgBox ('No case implemented for x' + IntToHex(expected, 2) + '!', 'fillInMonsterInt says:', MB_OK);
         raise EMessageAbort.Create;
      end;
   end; // end of CASE statement
end;

procedure fillInMonsterElementInt(const expected: byte; out theResult: integer);
begin
   if expected = 1 then
      theResult := 1
   else begin
      msgBox ('No case implemented for x' + IntToHex(expected, 2) + '!', 'fillInMonsterElementInt says:', MB_OK);
      raise EMessageAbort.Create;
   end;
end;

end.
