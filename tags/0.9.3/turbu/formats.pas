unit formats;
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
   classes;

type
   TProjectFormat = (pf_turbu, pf_2k, pf_2k3, pf_95, pf_sim, pf_xp, pf_vx);

   function scanRmFormat(database: TStream): TProjectFormat;

var
   GProjectFormat: TProjectFormat;

implementation
uses
   fileIO;

function scanRmFormat(database: TStream): TProjectFormat;
var
   i: Integer;
begin
   for i := $B to $19 do
      skipSec(i, database);
   if database.Position = database.Size then
      result := pf_2k
   else result := pf_2k3;
   database.Seek(0, soFromBeginning);
   getString(database);
end;

end.
