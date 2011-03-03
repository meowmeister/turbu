unit turbu_database_interface;
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
   DB;

type
   IRpgDatabase = interface(IInterface)
   ['{7C0AFF9B-9E51-4B67-AD08-5B8DAAF3EA24}']
   end;

   IRpgDatastore = interface
      function NameLookup(const name: string; id: integer): string;
   end;

implementation

end.
