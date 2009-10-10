unit rm2_turbu_maps;

interface
uses
   LMT, LMU, LDB,
   conversion_table, turbu_map_metadata, turbu_maps;

type
   T2k2RpgMap = class helper for TRpgMap
   public
      constructor Convert(base: TMapUnit; metadata: TMapTreeData; db: TLcfDataBase; id: smallint);
   end;

   T2k2MapRegion = class helper for TMapRegion
   public
      constructor Convert(base: TMapTreeData; id: smallint);
   end;

implementation
uses
   types, sysUtils, turbu_tilesets;

type
   ETileError = class(Exception);

   TDecodeResult = record
      baseTile: word;
      neighbors: TNeighbors;
      group: byte;
   end;

   TRpgMapHelper = class(TRpgMap);
   TMapRegionHelper = class(TMapRegion);

function decodeNeighbors(neighbors: byte): TNeighbors; forward;
function decode(const readID: integer): TDecodeResult; forward;
function convertDecodeResult(value: TDecodeResult): TTileRef; forward;

{ T2k2RpgMap }

constructor T2k2RpgMap.Convert(base: TMapUnit; metadata: TMapTreeData; db: TLcfDataBase; id: smallint);
var
   i: integer;
begin
   self.Create;
   self.name := string(metadata.name);
   self.id := id;
   self.tileset := string(db.getChipset(base.terrain).name);
   self.size := point(base.width, base.height);
   self.depth := 2;
   self.wraparound := TWraparound(base.unk0b);
   for I := 0 to high(base.lowChip) do
      self.tileMap[0][i] := convertDecodeResult(decode(base.lowChip[i]));
   for I := 0 to high(base.highChip) do
      self.tileMap[1][i] := convertDecodeResult(decode(base.highChip[i]));
   self.hasBackground := base.usesPano;
   if not base.hPan then
      self.hScroll := stNone
   else if not base.hPanAutoscroll then
      self.hScroll := stScroll
   else self.hScroll := stAutoscroll;
   if not base.vPan then
      self.vScroll := stNone
   else if not base.vPanAutoscroll then
      self.vScroll := stScroll
   else self.vScroll := stAutoscroll;
   self.scrollSpeed := point(base.hPanSpeed, base.vPanSpeed);
   self.battleCount := metadata.battles;
   for I := 0 to battleCount - 1 do
      self.battles[i] := (metadata.battle[i]);
   self.encounterScript := 'randomEncounterSteps';
   TRpgMapHelper(self).FEncounters[1] := metadata.encounterRate;
   self.regions := TRegionList.Create;
end;

{ T2k2MapRegion }

constructor T2k2MapRegion.Convert(base: TMapTreeData; id: smallint);
var
   i: integer;
begin
   self.Create;
   self.name := string(base.name);
   self.id := id;
   self.bounds := base.BoundsRect;
   self.battleCount := base.battles;
   for I := 0 to battleCount - 1 do
      self.battles[i] := (base.battle[i]);
   self.encounterScript := 'randomEncounterSteps';
   TMapRegionHelper(self).FEncounters[1] := base.encounterRate;
end;

{ Classless }

function decode(const readID: integer): TDecodeResult;
var
   refined, dummy: word;
   tilegroup: byte;
begin
   result.baseTile := 9999;
   if readID < 5000 then
      FillChar(result.neighbors, sizeof(TNeighbors), ord(either));

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
//animated tiles
      0..59:
      begin
         result.baseTile := refined mod 20; //let the drawing manager work this out
         if refined < 20 then
            result.group := 0
         else if refined < 40 then
            result.group := 2
         else result.group := 1;
      end;
      60..69: begin
         result.baseTile := refined - 60; //waterfall, etc
         result.group := 3;
      end;
//bordered tiles
      80..83:
      begin
         result.baseTile := refined - 80;
         result.group := 4;
      end;
      84..87:
      begin
         result.baseTile := refined - 84;
         result.group := 5;
      end;
      88..91:
      begin
         result.baseTile := refined - 88;
         result.group := 6;
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
         result.group := tilegroup;
      end;
      else
      begin
         raise Exception.createFmt('unknown tile: %d', [refined]);
      end;
   end;
end;

function decodeNeighbors(neighbors: byte): TNeighbors;
begin
//initialize
   FillChar(result, sizeof(TNeighbors), ord(no));

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
         FillChar(result, sizeof(TNeighbors), ord(yes));
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

function convertDecodeResult(value: TDecodeResult): TTileRef;
var
   neighbors: set of TDirs8;
   i: TDirs8;
begin
   //mark blank tile so we can nil it later on
   if (value.baseTile = 0) and (value.group = 10) then
   begin
      result.group := 255;
      result.tile := 255;
      Exit;
   end;

   result.group := value.group;
   if result.group in [0..2, 4..6] then
   begin
      neighbors := [];
      for i := low(TDirs8) to high(TDirs8) do
      begin
         if value.neighbors[i] = yes then
            include(neighbors, i);
      end;
      result.tile := byte(neighbors);
   end
   else result.tile := value.baseTile;
   case result.group of
      0..3: ;
      4..6:
      begin
         //swap #1 and #2 to get them to convert right
         if value.baseTile = 1 then
            value.baseTile := 2
         else if value.baseTile = 2 then
            value.baseTile := 1;
         result.group := 4 + ((result.group - 4) * 4) + value.baseTile;
      end;
      else inc(result.group, 9);
   end;
end;

end.
