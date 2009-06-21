unit chipset;
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
   sysUtils, classes, windows, //system libraries
   commons, fileIO, BER; //modules

type
   TChipSet = class(TObject)
   public
      incomplete: boolean;
      name: ansiString;
      filename: ansiString;
      terrain: array[0..161] of word;
      blockData: array[0..161] of byte;
      uBlockData: array[0..143] of byte;
      animation: boolean;
      hispeed: boolean;
      constructor Create(var theLDB: TStream; const id: word);
      function getName: ansiString;
      function empty: boolean;
      class procedure skip(theLDB: TStream; id: word);
   end;

implementation

{ Forward }
procedure fillInChipsetBool(const expected: byte; out theResult: integer); forward;

{ TChipSet }
constructor TChipSet.Create(var theLDB: TStream; const id: word);
var
   dummy: word;
   converter: intx80;
begin
   inherited Create;
try
with theLDB do
begin
   incomplete := false;
   converter := TBerConverter.Create(theLDB);
   dummy := converter.getData;
   if dummy <> id then
      raise EParseMessage.create('ChipSet section ' + intToStr(id) + ' of RPG_RT.LDB not found!');
   if peekAhead(theLDB, 0) = false then //blank chipset records just contain an x00 and nothing else
   begin
      name := getStrSec(1, theLDB, fillInBlankStr);
      if peekAhead(theLDB, 0) then
      begin
         incomplete := true;
         exit
      end;
      filename := getStrSec(2, theLDB, fillInBlankStr);
      if peekAhead(theLDB, 0) then
      begin
         incomplete := true;
         exit
      end;
      if peekAhead(theLDB, 3) then
      begin
         converter := TBerConverter.Create(theLDB);
         assert (converter.getData = 324, 'Chipset Terrain data segment size incorrect!');
         for dummy := 0 to 161 do
            read(terrain[dummy], 2);
      end
      else
         incomplete := true;
      if peekAhead(theLDB, 4) then
      begin
         converter := TBerConverter.Create(theLDB);
         assert (converter.getData = 162, 'Chipset Tile Data segment size incorrect!');
         for dummy := 0 to 161 do
            read(blockData[dummy], 1);
      end
      else
      begin
         incomplete := true;
         for dummy := 0 to 161 do
            blockData[dummy] := $0F;
      end;
      if peekAhead(theLDB, 5) then
      begin
         converter := TBerConverter.Create(theLDB);
         assert (converter.getData = 144, 'Chipset Tile Data segment size incorrect!');
         for dummy := 0 to 143 do
            read(ublockData[dummy], 1);
      end
      else
      begin
         incomplete := true;
         ublockData[1] := $1F;
         for dummy := 2 to 143 do
            ublockData[dummy] := $0F;
      end;
      animation := getChboxSec($0b, theLDB, @fillInChipsetBool);
      hispeed := getChboxSec($0c, theLDB, @fillInChipsetBool);
      dummy := 0;
      Read(dummy, 1);
      if dummy <> 0 then
         raise EParseMessage.create('Chipset section ' + intToStr(id) + ' final 0 not found');
   end //end of IF block
   else
   begin
      incomplete := true;
      for dummy := 0 to 161 do
         blockData[dummy] := $0F;
      ublockData[0] := $1F;
      for dummy := 1 to 143 do
         ublockData[dummy] := $0F;
   end;
end; // end of WITH block
except
   on E: EParseMessage do
   begin
      msgBox(E.message, 'TChipSet.Create says:', MB_OK);
      raise EMessageAbort.Create
   end
end; // end of TRY block
end;

function TChipSet.getName: ansiString;
begin
   result := name;
end;

function TChipSet.empty: boolean;
var
   i: byte;
begin
   result := incomplete;
   if name <> '' then result := false;
   if filename <> '' then result := false;
   for i := 1 to 162 do
   begin
      if terrain[i] <> 0 then result := false;
      if blockData[i] <> $0F then result := false;
      if (i <= 144) and (i > 1) and (uBlockData[i] <> $0F) then result := false;
   end;
   if animation or hispeed = true then result := false;
end;

class procedure TChipSet.skip(theLDB: TStream; id: word);
var
   dummy, i: byte;
begin
try
with theLDB do
begin
   Read(dummy, 1);
   if dummy <> id then
      raise EParseMessage.create('ChipSet section ' + intToStr(id) + ' of RPG_RT.LDB not found!');
   if peekAhead(theLDB, 0) = false then //blank chipset records just contain an x00 and nothing else
   begin
      skipSec(1, theLDB);
      if peekAhead(theLDB, 0) then
         exit;
      skipSec(2, theLDB);
      if peekAhead(theLDB, 0) then
         exit;
      for i := 3 to 5 do
         skipSec(i, theLDB);
      skipSec($0b, theLDB);
      skipSec($0c, theLDB);
      Read(dummy, 1);
      if dummy <> 0 then
         raise EParseMessage.create('Chipset section ' + intToStr(id) + ' final 0 not found');
   end;
end; // end of WITH block
except
   on E: EParseMessage do
   begin
      msgBox(E.message, 'TChipSet.Skip says:', MB_OK);
      raise EMessageAbort.Create
   end
end // end of TRY block
end;

{ Classless }

procedure fillInChipsetBool(const expected: byte; out theResult: integer); //handleUnex
begin
   case expected of
      $0b, $0c: theResult := integer(false);
   else
      begin
         msgBox ('No case implemented for x' + IntToHex(expected, 2) + '!', 'fillInChipsetBool says:', MB_OK);
         raise EMessageAbort.Create;
      end;
   end; // end of CASE statement
end;

end.
