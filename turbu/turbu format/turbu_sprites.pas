unit turbu_sprites;
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

//note: sprite file names should not contain a comma
interface
uses
   commons;

type
   TMoveMatrix = array of packed array of byte;

   TSpriteData = record
      name: string;
      moveMatrix: integer;
   end;

   function extractSpriteData(value: string): TSpriteData;
   function nextPosition(matrix: TMoveMatrix; var current: TRpgPoint): byte;

implementation
uses
   sysUtils, types,
   strtok;

function extractSpriteData(value: string): TSpriteData;
var
   index: integer;
begin
   index := 1;
   result.name := strtok.GetNextToken(value, ',', index);
   result.moveMatrix := strToInt(strtok.GetNextToken(value, ',', index));
end;

function nextPosition(matrix: TMoveMatrix; var current: TRpgPoint): byte;
begin
   if (current.x > high(matrix)) or (current.y > high(matrix[current.x])) then
      current := point(0, 0)
   else begin
      inc(current.y);
      if (current.y > high(matrix[current.x])) then
         current.y := 0;
   end;
   result := matrix[current.x, current.y];
end;

end.