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
unit turbu_script_interface;

interface

type
   IScriptEngine = interface(IInterface)
   ['{60799F2F-0C1F-491C-869D-1996AF8D858F}']
   end;

   IDesignScriptEngine = interface(IScriptEngine)
   ['{A3B2746C-F3D1-4B61-B9D8-9D8E82229979}']
   end;
implementation

end.
