unit turbu_map_interface;
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
type
   IRpgMap = interface(IInterface)
   ['{8B9CDCC2-AFB6-408C-88C3-2E50D145C901}']
      function GetTileset: string;
      procedure SetTileset(const Value: string);
      property tileset: string read GetTileset write SetTileset;
   end;

implementation

end.
