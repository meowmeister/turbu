unit chipset_data;
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

uses windows, forms, sysUtils, controls, dialogs, //system libs
     commons, chipset; //turbu libs

const
   tilesetMap: array[0..12] of string = ('water', 'water3', 'water2', 'anims',
               'borders5', 'borders6', 'borders7', 'lowtile8', 'lowtile9',
               'lowtile10', 'hitile1', 'hitile2', 'hitile3');
const
   TILESET: tpoint = (x: 96; y: 128);
   TINY_TILESET: TPoint = (x:48; y:64);
   SIZ_TILE: tpoint = (x:16; y:16);
   TINY_TILE: tpoint = (x:8; y:8);

type
   dirs8 = (n, ne, e, se, s, sw, w, nw);
   troolean = (no, yes, either);
   whichLayer = (lower, upper);
   TButtonCode = (btn_enter, btn_cancel, btn_up, btn_down, btn_left, btn_right);
   PButtonCode = ^TButtonCode;
   ANeighbors = packed array [dirs8] of troolean;
   TGameState = (on_map, in_message, in_menu, in_battle, sleeping, gs_fading);
   TSwitchState = (sw_noSwitch, sw_ready, sw_switching);

   TDecodeResult = record
      baseTile: word;
      neighbors: ANeighbors;
      group: string;
   end;

   TTileMap = class; //forward declaration to allow FNeighbors to work;

   TTileData = class(TObject)
   private
      FTileID: word; // which tile from the overall x,y grid of the chipset
      FBaseTile: word; // which tile from the RM2K editor selection
      FGridLoc: TPoint; //where this tile is located on the map
      FTileLayer: whichlayer; //high chip or low chip
      FChanged: boolean; //has the tile been modified in the editor?
   public
      procedure Assign (const value: TTileData);
      function neighbors(owner: TTileMap): ANeighbors; //determines which of the
                                                       //46 patterns it fits
      property id: word read FTileID write FTileID;
      property base: word read FBaseTile write FBaseTile;
      property location: TPoint read FGridLoc write FGridLoc;
      property layer: whichlayer read FTileLayer write FTileLayer;
      property changed: boolean read FChanged write FChanged;
   end;


   TTileMap = class(TObject)
   private
      FTiles: array of array of TTileData;
      FHeight: integer;
      FWidth: integer;
      FFileName: string;
      procedure writeTile (x,y: integer; const theTile: TTileData);
      function getTile (x,y: integer): TTileData;
   public
      property tile [x,y: integer]: TTileData read getTile write writeTile; default;
      property height: integer read FHeight;
      property width: integer read FWidth;
      property filename: string read FFileName write FFileName;
      constructor create (x,y: integer);
      procedure loadChipset(const filename: string; var chipList:TImageList);
   end;

{Classless}
function decodeNeighbors(neighbors: byte): ANeighbors;
function decode (const readID: integer): TDecodeResult;
function decodeZOrder(const tileID: integer; passInfo: integer): integer;
function decodePassing(const tileID: integer; const chipData: TChipSet): integer;
function decodeTerrain(const tileID: integer; const chipData: TChipSet): word;

implementation

{ TTileData }

procedure TTileData.assign(const value: TTileData);
begin
   FTileID:= value.id;
   FBaseTile:= value.base;
   FGridLoc:= value.location;
   FTileLayer:= value.layer;
end;

function TTileData.neighbors(owner: TTileMap): ANeighbors;
begin
//do later
end;

function decode(const readID: integer): TDecodeResult;
var
   i: dirs8;
   refined, dummy: word;
   tilegroup: byte;
begin
try
try
   result.baseTile := 9999;
   if readID < 5000 then
      for i := n to nw do
         result.neighbors[i] := either;
      //end for
   //end if

//first decoding: simplify base tile a bit
   if readID >= 5000 then
   begin
      result.baseTile := readID mod 5000;
      if readID >= 10000 then
         inc(result.baseTile, 300)
      else inc(result.baseTile, 100);
   end
   else
      result.baseTile := readID div 50;
   //end if
//find neighbor flags, if applicable
   if result.baseTile < 100 then
      result.neighbors := decodeNeighbors(readID mod 50);
//second decoding: find base tile, tile group style
   refined := result.baseTile;
   case refined of
{Examples are based on outline.png from the RTP}
//animated tiles
      0..59:
      begin
         result.baseTile := refined mod 20; //let the drawing manager work this out
         if refined < 20 then
            result.group := tilesetMap[0]
         else if refined < 40 then
            result.group := tilesetMap[2]
         else result.group := tilesetMap[1];;
      end;
      60..69: begin
         result.baseTile := refined - 60; //waterfall, etc
         result.group := tilesetMap[3];
      end;
//bordered tiles
      80..83:
      begin
         result.baseTile := refined - 80;
         result.group := tilesetMap[4];
      end;
      84..87:
      begin
         result.baseTile := refined - 84;
         result.group := tilesetMap[5];
      end;
      88..91:
      begin
         result.baseTile := refined - 88;
         result.group := tilesetMap[6];
      end;
//non-mobile tiles
      100..243, 300..443:
      begin
//initialization
         dummy := refined - 100;
         result.baseTile := 0;
         tilegroup := 7;
//correction for high chips and different columns/groups
         if dummy >= 200 then
         begin
            inc(tilegroup, 3);
            dec(dummy, 200);
         end;
         if dummy >= 48 then
         begin
            inc(tilegroup, dummy div 48);
            dummy := dummy mod 48;
         end;
//now do the math;
         inc(result.baseTile, dummy); //row
         result.group := tilesetMap[tilegroup];
      end;
      else
      begin
         raise EParseMessage.create('unknown tile: ' + intToStr(refined));
      end;
   end;
except
   on E: EParseMessage do
   begin
      msgBox(E.message, 'TGameForm.FormShow says:', MB_OK);
      raise EMessageAbort.Create
   end
end; // end of TRY block
except
   on EMessageAbort do
   begin
      ShowMessage('Aborting due to fatal error.');
      application.Terminate
   end
end // end of second TRY block
end;

{*********************************************************************
* Decodes RM2K's obnoxious neighbor flags.
*********************************************************************}
function decodeNeighbors(neighbors: byte): ANeighbors;
var i: dirs8;
begin
//initialize
   for i := n to nw do
      result[i] := no;

   case neighbors of
      0:; //do nothing
      1..15:
//easiest case: use binary logic to set the 4 corner flags if that's all
//there is
      begin
         if neighbors >= 8 then
         begin
            result[sw] := yes;
            dec(neighbors, 8);
         end;
         if neighbors >= 4 then
         begin
            result[se] := yes;
            dec(neighbors, 4);
         end;
         if neighbors >= 2 then
         begin
            result[ne] := yes;
            dec(neighbors, 2);
         end;
         if neighbors = 1 then
            result[nw] := yes;
      end;
//one side blocks
{
?.. 16  ?.\ 17  ?.. 18  ?.\ 19
|..     |..     |..     |..
?..     ?..     ?./     ?./
}
      16..19:
      begin
         result[w] := yes;
         result[nw] := either;
         result[sw] := either;
         dec(neighbors, 16);
         if neighbors >= 2 then
         begin
            result[se] := yes;
            dec(neighbors, 2);
         end;
         if neighbors = 1 then
            result[ne] := yes;
        //end if
      end;
{
?-? 20  ?-? 21  ?-? 22  ?-? 23
...     ...     ...     ...
...     ../     \..     \./
}
      20..23:
      begin
         result[n] := yes;
         result[nw] := either;
         result[ne] := either;
         dec(neighbors, 20);
         if neighbors >= 2 then
         begin
            result[sw] := yes;
            dec(neighbors, 2);
         end;
         if neighbors = 1 then
            result[se] := yes;
        //end if
      end;
{
..? 24  ..? 25  /.? 26  /.? 27
..|     ..|     ..|     ..|
..?     \.?     ..?     \.?
}
      24..27:
      begin
         result[e] := yes;
         result[se] := either;
         result[ne] := either;
         dec(neighbors, 24);
         if neighbors >= 2 then
         begin
            result[nw] := yes;
            dec(neighbors, 2);
         end;
         if neighbors = 1 then
            result[sw] := yes;
        //end if
      end;
{
... 28  /.. 29  ..\ 30  /.\ 31
...     ...     ...     ...
?-?     ?-?     ?-?     ?-?
}
      28..31:
      begin
         result[s] := yes;
         result[sw] := either;
         result[se] := either;
         dec(neighbors, 28);
         if neighbors >= 2 then
         begin
            result[ne] := yes;
            dec(neighbors, 2);
         end;
         if neighbors = 1 then
            result[nw] := yes;
        //end if
      end;

//playtime's over
{
?.? 32  ?-? 33
|.|     ...
?.?     ?-?
}
      32..33:
      begin
         if neighbors = 32 then
         begin
            result[e] := yes;
            result[w] := yes;
         end
         else //case 33
         begin
            result[n] := yes;
            result[s] := yes;
         end;
         result[nw] := either;
         result[ne] := either;
         result[sw] := either;
         result[se] := either;
      end;
{
/-? 34  /-? 35  ?-\ 36  ?-\ 37
|..     |..     ..|     ..|
...     ../     ...     \..
}
      34..35:
      begin
         result[w] := yes;
         result[nw] := yes;
         result[n] := yes;
         result[ne] := either;
         if neighbors = 35 then
            result[se] := yes;
      end;
      36..37:
      begin
         result[e] := yes;
         result[ne] := yes;
         result[n] := yes;
         result[nw] := either;
         if neighbors = 37 then
            result[sw] := yes;
      end;
{
... 38  /.. 39  ... 40  ..\ 41
..|     ..|     |..     |..
?-/     ?-/     \-?     \-?
}
      38..39:
      begin
         result[e] := yes;
         result[se] := yes;
         result[s] := yes;
         result[sw] := either;
         if neighbors = 39 then
            result[nw] := yes;
      end;
      40..41:
      begin
         result[w] := yes;
         result[sw] := yes;
         result[s] := yes;
         result[se] := either;
         if neighbors = 41 then
            result[ne] := yes;
      end;
{
/-\ 42  /-? 43  ?.? 44  ?-\ 45
|.|     |..     |.|     ..|
?.?     \-?     \-/     ?-/

/-\ 46
|.|
\-/
}
      42..46:
      begin
         for i := n to nw do
            result[i] := yes;
         case neighbors of
            42:
            begin
               result[s] := no;
               result[sw] := either;
               result[se] := either;
            end;
            43:
            begin
               result[e] := no;
               result[ne] := either;
               result[se] := either;
            end;
            44:
            begin
               result[n] := no;
               result[nw] := either;
               result[ne] := either;
            end;
            45:
            begin
               result[w] := no;
               result[nw] := either;
               result[sw] := either;
            end;
         end;//end of 42..25 sub-CASE statement
      end;//end of 42..25 case
   end;//end of big CASE statement
end;

{ TTileMap }

constructor TTileMap.create(x, y: integer);
var i,j: word;
begin
   inherited create;
   setLength (FTiles, y, x);
   FHeight := x-1;
   FWidth := y-1;
   for i := 0 to FWidth do
      for j := 0 to FHeight do
        FTiles[i,j] := TTileData.create;
      //end
   //end
end;

function TTileMap.getTile(x, y: integer): TTileData;
begin
   result := FTiles[x,y];
end;

procedure TTileMap.loadChipset(const filename: string;  var chipList: TImageList);
begin
//do later
end;

procedure TTileMap.writeTile(x, y: integer; const theTile: TTileData);
begin
   FTiles[x,y].Assign(theTile);
end;

function decodeZOrder(const tileID: integer; passInfo: integer): integer;
begin
   if (passInfo and $30 = $30) then
{      if (tileID mod 50 = 0) and (tileID < 5000) then
         result := 0
      else result := 6}
      result := 10
      //end if
   else if passInfo and $10 = $10 then
      result := 6
   else result := 1;

   if tileID >= 10000 then
      inc(result);
end;

function decodePassing(const tileID: integer; const chipData: TChipSet): integer;
begin
   if tileID >= 10000 then
      result := chipData.ublockData[tileID - 10000]
   else if tileID >= 5000 then
      result := chipData.blockData[(tileID - 5000) + 18]
   else if tileID >= 4000 then
      result := chipData.blockData[((tileID - 4000) div 50) + 6]
   else if tileID >= 3000 then
      result := chipData.blockData[((tileID - 3000) div 50) + 3]
   else
      result := chipData.blockData[tileID div 1000];
end;

function decodeTerrain(const tileID: integer; const chipData: TChipSet): word;
begin
   result := 0;
   if tileID >= 10000 then
      assert(false)
   else if tileID >= 5000 then
      result := chipData.terrain[(tileID - 5000) + 18]
   else if tileID >= 4000 then
      result := chipData.terrain[((tileID - 4000) div 50) + 6]
   else if tileID >= 3000 then
      result := chipData.terrain[((tileID - 3000) div 50) + 3]
   else
      result := chipData.terrain[tileID div 1000];
end;

end.
