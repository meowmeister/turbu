unit ps_pointer;
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

(*It's necessary to store and retrieve the location of uPSRuntime.MyAllMethodsHandler,
but it gets thunked by the package system, which breaks the code.  Putting this
function inside the same package as uPSRuntime can work around the issue. *)
   function mamhLoc: pointer;

implementation
uses
   uPSRuntime;

var
   allMethodsHandler: pointer;

function mamhLoc: pointer;
begin
   result := allMethodsHandler;
end;

initialization
   allMethodsHandler := @uPSRuntime.MyAllMethodsHandler;
end.
