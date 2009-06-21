unit LMU;
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

uses classes, //system libraries
     events, LDB, LMT; //modules

type
{*********************************************************************
* MapUnit class.  This object stores the LMU entry for one map.
* I know it's sorta bad coding to make all variables public, but it's
* the only way to avoid the necessity of making ten bajillion accessors.
*********************************************************************}
   TMapUnit = class (TObject)
   private
      FEvents: TEventBlock;
      FMapID: smallint;
      FChipsetname: ansiString;

      function FEventlen: word;
      function getEvent(x: word): TEvent;
      procedure setEvent(x: word; input: TEvent);
{$IFDEF ENGINE}
      function countEventTiles: word;
      function countCharacterEvents: word;
{$ENDIF}
   public
      terrain: word;
      width: word;
      height: word;
      unk0b: byte;
      usesPano: boolean;
      panoName: ansiString;
      hPan: boolean;
      vPan: boolean;
      hPanAutoscroll: boolean;
      hPanSpeed: shortint;
      vPanAutoscroll: boolean;
      vPanSpeed: shortint;
      lowChip: array of word;
      highChip: array of word;
      eventData: ansiString;
      modified: integer;

      constructor Create(theLMU: TStream; database: TLcfDataBase; tree: TFullTree; id: word);
      destructor Destroy; override;
      property eventCount: word read FEventlen;
      property events[x: word]: TEvent read getEvent write setEvent;
{$IFDEF ENGINE}
      property eventTiles: word read countEventTiles;
      property characterCount: word read countCharacterEvents;
{$ENDIF}
      property eventBlock: TEventBlock read FEvents write FEvents;
      property mapID: smallint read FMapID;
      property chipsetName: ansiString read FChipsetName;
   end;

   procedure fillInLmuInt(const expected: byte; out theResult: integer);
   procedure fillInLmuStr(const expected: byte; out theResult: ansiString);

implementation

uses windows, sysUtils, //system libs
     commons, formats, BER, fileIO; //turbu libs

{*********************************************************************
* The only member function this object really needs, the constructor takes
* a fileStream containing an LMU file as a parameter and reads in the
* record from the file.
*********************************************************************}
constructor TMapUnit.Create(theLMU: TStream; database: TLcfDataBase; tree: TFullTree; id: word);
var
   i, len: integer;
begin
try
   inherited Create;
   FMapID := id;
   terrain := getNumSec(1, theLMU, fillInLmuInt);
   width := getNumSec(2, theLMU, fillInLmuInt);
   Height := getNumSec(3, theLMU, fillInLmuInt);
   unk0b := getNumSec($b, theLMU, fillInLmuInt); //not sure what this is for, but it's there
   usesPano := getChboxSec($1F, theLMU, fillInLmuInt);
   PanoName := getStrSec($20, theLMU, fillInLmuStr);
   hPan := getChboxSec($21, theLMU, fillInLmuInt);
   vPan := getChboxSec($22, theLMU, fillInLmuInt);
   hPanAutoscroll := getChboxSec($23, theLMU, fillInLmuInt);
   hPanSpeed := getNumSec($24, theLMU, fillInLmuInt);
   vPanAutoscroll := getChboxSec($25, theLMU, fillInLmuInt);
   vPanSpeed := getNumSec($26, theLMU, fillInLmuInt);
   len := width * height;
   setLength(lowChip, len);
   setLength(highChip, len);
   if GProjectFormat = pf_2k3 then
   begin
      for i := $2A to $2D do
         skipSec(i, theLMU);
      for i := $32 to $34 do
         skipSec(i, theLMU);
      for i := $3C to $3E do
         skipSec(i, theLMU);
   end;
   getArraySec($47, theLMU, lowChip[0]);
   getArraySec($48, theLMU, highChip[0]);
   eventData := getStrSec($51, theLMU, fillInLmuStr);
   FEvents := TEventBlock.create(eventData);
   modified := getNumSec($5b, theLMU, fillInLmuInt);
   assert(peekAhead(theLMU, 0));
   FChipsetName := database.getChipset(self.terrain).filename;
//read chip data and event section
except
   on E: EParseMessage do
   begin
      msgBox(E.message, 'TMapTreeData.Create says:', MB_OK);
      raise EMessageAbort.Create
   end
end //end of TRY block
end;

destructor TMapUnit.Destroy;
begin
   FEvents.Free;
   inherited;
end;

function TMapUnit.FEventlen: word;
begin
   result := FEvents.len;
end;

function TMapUnit.getEvent(x: word): TEvent;
begin
   result := FEvents[x];
end;

{$IFDEF ENGINE}
function TMapUnit.countCharacterEvents: word;
var
   i: word;
begin
   result := 0;
   for i := 0 to FEvents.len - 1do
      if not(FEvents[i].isTile) then
         inc(result);
end;

function TMapUnit.countEventTiles: word;
var
   i: word;
begin
   result := 0;
   for i := 0 to FEvents.len - 1do
      if FEvents[i].isTile then
         inc(result);
end;
{$ENDIF}

procedure TMapUnit.setEvent(x: word; input: TEvent);
begin
//do this later
assert(false);
end;


{ Classless }

procedure fillInLmuInt(const expected: byte; out theResult: integer);
begin
   case expected of
      1: theResult := 1;
      2: theResult := 20;
      3: theResult := 15;
      $1F, $20..$23, $25: theResult := integer(false);
      $24, $26: theResult := 0;
   else
      begin
         msgBox ('No case implemented for x' + IntToHex(expected, 2) + '!', 'fillInLmuInt says:', MB_OK);
         raise EMessageAbort.Create
      end
   end;
end;

procedure fillInLmuStr(const expected: byte; out theResult: ansiString);
begin
   case expected of
      1, $20: theResult := '';
   else
      begin
         msgBox ('No case implemented for x' + IntToHex(expected, 2) + '!', 'FillInLmtStr says:', MB_OK);
         raise EMessageAbort.Create
      end
   end;
end;

end.
