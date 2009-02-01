unit LMT;
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
uses windows, classes, //system libraries
     fileIO, charset_data, rm_sound; //modules

   type

      {*********************************************************************
      * MapTreeData class.  This object stores the LMT entry for one map.
      * I know it's sorta bad coding to make all variables public, but it's
      * the only way to avoid the necessity of making ten bajillion accessors.
      *********************************************************************}
      TMapTreeData = class (TObject)
      private
         FBgmData: TRmMusic;
      public
         name: ansiString;
         parent: word;
         generation: byte;
         isArea: boolean;
         unk05: integer;
         unk06: integer;
         treeOpen: boolean;
         bgmState: radioSet;
         bgmName: ansiString;
         battleBgState: radioSet;
         battleBgName: ansiString;
         canPort: radioSet;
         canEscape: radioSet;
         canSave: radioSet;
         battles: word;
         battle: array of word;
         encounterRate: word;
         areaData: array [1..4] of integer;
         constructor Create(theLMT: TStream; id: word);
         destructor Destroy; override;

         property bgmData: TRmMusic read FBgmData;
      end;

      {*********************************************************************
      * FullTree class.  This class contains all of the entries in the LMT.
      * mapSet and projectLen have contain the data entries and the total number
      * of elements in mapSet, respectively.
      *********************************************************************}
      TFullTree = class (TObject)
      private
         projectLen: word;
         mapSet: array of TMapTreeData;
         FHeroStartMap: smallint;
         FHeroStartX, FHeroStartY: word;
         FVhStartMap: array[TVehicleSet] of smallint;
         FVhStartX, FVhStartY: array[TVehicleSet] of word;
         currentMap: word;
         inserted: boolean;
         translationTable: array of word;
         function getVehicleStartX(x: TVehicleSet): word;
         function getVehicleStartY(x: TVehicleSet): word;
         function getVehicleStartMap(x: TVehicleSet): smallint;
      public
         nodeSet: array of word;
         constructor Create(theLMT: TStream);
         destructor Destroy; override;
         function getMapData(id: word): TMapTreeData;
         function getMax: word;
         function getSize: word;
         procedure searchBack(var id: word);
         procedure searchForward(var id: word);
         function getBgm(const whichMap: word): ansiString;
         function lookup(x: word): word;

         property items[x: word]: TMapTreeData read getMapData; default;
         property vhStartMap[x: TVehicleSet]: smallint read getVehicleStartMap;
         property vhStartX[x: TVehicleSet]: word read getVehicleStartX;
         property vhStartY[x: TVehicleSet]: word read getVehicleStartY;
         property heroStartMap: smallint read FHeroStartMap;
         property heroStartX: word read FHeroStartX;
         property heroStartY: word read FHeroStartY;
      end;

implementation

uses strUtils, sysUtils, //windows libs
     commons, BER; //turbu libs

{Forwards}
procedure fillInLmtInt(const expected: byte; out theResult: integer); forward;
procedure fillInLmtEndInt(const expected: byte; out theResult: integer); forward;

{ TMapTreeData}

{*********************************************************************
* The only member function this object really needs, the constructor takes
* a fileStream containing an RPG_RT.LMT file as a parameter and reads in the
* record from the file.
*********************************************************************}
constructor TMapTreeData.Create(theLMT: TStream; id: word);
var
   dummy: byte;
   i: word;
begin
try
   inherited Create;
   name := getStrSec (1, theLMT, fillInBlankStr);
   parent := getNumSec(2, theLMT, fillInLmtInt);
   generation := getNumSec(3, theLMT, fillInLmtInt);
   isArea := not getChboxSec(4, theLMT, fillInLmtInt); //for some reason, the
                                //format is backwards here.  The NOT fixes that.
   unk05 := getNumSec(5, theLMT, fillInLmtInt);
   unk06 := getNumSec(6, theLMT, fillInLmtInt);
   treeOpen := getChboxSec(7, theLMT, fillInLmtInt);
   bgmState := getRsetSec($0b, theLMT, fillInLmtInt);
   FBgmData := TRmMusic.Create($0C, theLMT);
   bgmName := FBgmData.filename;

   battleBGState := getRsetSec($15, theLMT, fillInLmtInt);
   if battleBgState = third then              //$16 is an optional section, only
      battleBgName := getStrSec($16, theLMT, fillInBlankStr); //included if necessary.
   canPort := getRSetSec($1F, theLMT, fillInLmtInt);
   canEscape := getRSetSec($20, theLMT, fillInLmtInt);
   canSave := getRSetSec($21, theLMT, fillInLmtInt);

   {The following is a kludgy workaround involving the list header for the
   battle section.}
   theLMT.read(dummy, 1);
   if dummy <> $29 then
      raise EParseMessage.create('LMT field x29 not found for section ' + intToStr(id));
   intX80.Create(theLMT).free;
   theLMT.read(dummy, 1);
   battles := dummy;

   {reads the section header, then uses getNumSec to retrieve the battle record.
   The records seem to be treated as lists; perhaps there will be a future problem
   found involving a second record.}
   SetLength(battle, battles);
   for i := 1 to battles do
   begin
      theLMT.Read(dummy, 1);
      if dummy <> i then
         raise EParseMessage.create('LMT encounter record ' + intToStr(i) + 'not found in section ' + intToStr(id));
      battle[i - 1] := getNumSec(1, theLMT, @fillInLmtInt);
      theLMT.Read(dummy, 1);
      if dummy <> 0 then
         raise EParseMessage.create('LMT x29 final section padding of \0 not found for section ' + intToStr(id) + ', encounter #' + intToStr(i))
   end;

   encounterRate := getNumSec($2C, theLMT, @fillInLmtInt);
   theLMT.Read(dummy, 1);
   if dummy = $33 then
   begin
      theLMT.Read(dummy, 1);
      if dummy <> $10 then
         raise EParseMessage.create('Error: Section x33 length was set at' + intToHex(dummy, 2) + ' instead of x10!')
      else
      begin
         for dummy := 1 to 4 do
            theLMT.Read(areaData[dummy], 4)
      end
   end
   else if dummy > $33 then
   begin
      raise EParseMessage.create('Error: Expected section x33 not found!');
   end
   else if dummy < 33 then
   begin
      raise EParseMessage.create('Error: Expected section x33, but found section x' + intToHex(dummy, 2) + ' stuck in there first.');
   end;
   theLMT.read(dummy, 1);
   if dummy <> 0 then
      raise EParseMessage.create('LMT section ' + intToStr(id) + ' final 0 not found.')
except
   on E: EParseMessage do
   begin
      msgBox(E.message, 'TMapTreeData.Create says:', MB_OK);
      raise EMessageAbort.Create
   end
end //end of TRY block
end;

destructor TMapTreeData.Destroy;
begin
   FBgmData.Free;
   finalize(battle);
   inherited;
end;

constructor TFullTree.Create(theLMT: TStream);
var
//   dummy: byte;
   dummy16: word;
   i: word;
   converter: intX80;
   v: TVehicleSet;
begin
try
   inherited Create;
   inserted := false;
   converter := intX80.Create (theLMT);
   projectLen := converter.getData;
   SetLength(mapSet, projectLen);
   setLength(translationtable, projectLen);
   projectLen := projectLen - 1;
   for i := 0 to projectLen do
   begin
      converter.read(theLMT);
      dummy16 := converter.getData;
      if ((i = 0) and (dummy16 = 0)) or (dummy16 >= translationtable[i - 1]) then
      begin
         mapSet[i] := TMapTreeData.Create(theLMT, i);
         translationtable[i] := dummy16;
      end
      else raise EParseMessage.create('LMT section ' + intToStr(i) + ' not found!')
   end;
   converter.read(theLMT);
   dummy16 := converter.getData;
   assert (dummy16 = projectLen + 1);
   setLength(nodeSet, projectLen + 1);
   for i := 0 to projectLen do
   begin
      converter.read(theLMT);
      nodeSet[i] := converter.getData
   end;
   assert(nodeSet[0] = 0);
   converter.read(theLMT);
   currentMap := converter.getData;
   converter.free;
   FHeroStartMap := getNumSec(1, theLMT, @fillInLmtEndInt);
   FHeroStartX := getNumSec(2, theLMT, @fillInLmtEndInt);
   FHeroStartY := getNumSec(3, theLMT, @fillInLmtEndInt);
   i := 3;
   for v := low(TVehicleSet) to high (TVehicleSet) do
   begin
      inc(i, 8);
      FVhStartMap[v] := getNumSec(i, theLMT, fillInLmtEndInt);
      inc(i);
      FVhStartX[v] := getNumSec(i, theLMT, fillInLmtEndInt);
      inc(i);
      FVhStartY[v] := getNumSec(i, theLMT, fillInLmtEndInt);
   end;
   assert(peekAhead(theLMT, 0));
except
   on E: EParseMessage do
   begin
      msgBox(E.message, 'TFullTree.Create says:', MB_OK);
      raise EMessageAbort.Create
   end
end; //end of TRY block
end;

function TFullTree.getBgm(const whichMap: word): ansiString;
var currentMap: word;
begin
   currentMap := whichMap;
   result := '';
   repeat
      if (self[currentMap].bgmName <> '') and (self[currentMap].bgmName <> '(OFF)') then
         result := self[currentMap].bgmName
      else currentMap := self[currentMap].parent;
   until (currentMap = 0) or (result <> '');
end;

function TFullTree.getMapData(id: word): TMapTreeData;
var
   i: word;
begin
   i := 0;
   while translationTable[i] <> id do
      inc(i);
   result := mapSet[i]
end;

function TFullTree.getMax: word;
begin
   result := projectLen;
   while (mapSet[result].isArea = true) and (mapSet[result].name <> '') do
      result := result - 1;
end;

function TFullTree.getSize: word;
begin
   result := projectLen;
end;

function TFullTree.getVehicleStartMap(x: TVehicleSet): smallint;
begin
   result := FVhStartMap[x];
end;

function TFullTree.getVehicleStartX(x: TVehicleSet): word;
begin
   result := FVhStartX[x];
end;

function TFullTree.getVehicleStartY(x: TVehicleSet): word;
begin
   result := FVhStartY[x];
end;

procedure TFullTree.searchBack(var id: word);
begin
   repeat
      id := id - 1
   until mapSet[id].isArea = false
end;

procedure TFullTree.searchForward(var id: word);
begin
   repeat
      id := id + 1
   until mapSet[id].isArea = false
end;

function TFullTree.lookup(x: word): word;
begin
   result := translationtable[x];
end;

destructor TFullTree.Destroy;
var i: integer;
begin
   for i := low(mapSet) to high(mapSet) do
      mapSet[i].Free;
   finalize(mapSet);
   finalize(translationTable);
   inherited Destroy;
end;

procedure fillInLmtInt(const expected: byte; out theResult: integer);
begin
   case expected of
      2, 3, 5, 6: theResult := 0;
      7: theResult := integer(false);
      $2C: theResult := 25;
   else
      begin
         msgBox ('No case implemented for x' + IntToHex(expected, 2) + '!', 'FillInLmtInt says:', MB_OK);
         raise EMessageAbort.Create
      end
   end;
end;

procedure fillInLmtEndInt(const expected: byte; out theResult: integer);
begin
   case expected of
      1, $B, $15, $1F: theResult := -1;
      2, 3, $C, $D, $16, $17, $20, $21: theResult := 0;
   else
      begin
         msgBox ('No case implemented for x' + IntToHex(expected, 2) + '!', 'FillInLmtEndInt says:', MB_OK);
         raise EMessageAbort.Create
      end
   end;
end;


end.
