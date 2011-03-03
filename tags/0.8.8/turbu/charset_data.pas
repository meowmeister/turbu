unit charset_data;
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

uses sg_defs, turbu_defs;

type
   TAnimFrame = (left, center, right);
   TCharTiles = (bottom, top);
   TVehicleSet = (vh_boat, vh_ship, vh_airship);

const
   SPRITE: TSgPoint = (x: 24; y: 16);
   SPRITE_SET: TSgPoint = (x: 288; y: 256);

function opposite_facing(const whichDir: TFacing): TFacing; inline;
function towards(location, target: TSgPoint): TFacing;

implementation

function opposite_facing(const whichDir: TFacing): TFacing;
begin
   result := TFacing((ord(whichDir) + 2) mod 4);
end;

function towards(location, target: TSgPoint): TFacing;
var
   dX, dY: integer;
begin
   dX := location.x - target.X;
   dY := location.y - target.y;
   if abs(dX) > abs(dY) then
   begin
      if dX > 0 then
         result := facing_left
      else result := facing_right;
   end else begin
      if dY > 0 then
         result := facing_up
      else result := facing_down;
   end;
end;

end.
