unit turbu_debugging;
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

   procedure DebugExceptionHandler(value: pointer);

implementation

uses
   sysUtils, classes;

type
   TExceptionProc = procedure(value: pointer);

var
   oldException: TExceptionProc;

procedure DebugExceptionHandler(value: pointer);
var
   E: Exception;
begin
   e := Exception(value);
   AcquireExceptionObject;
   asm int 3 end; //manual breakpoint
   e.Free;
end;

{$IFDEF DEBUG_EXCEPTIONS}
initialization
begin
   oldException := @raiseExceptionProc;
   RaiseExceptionProc := @DebugExceptionHandler;
end;
{$ENDIF}

end.
