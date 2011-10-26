unit logs;
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

   procedure logText(data: string);
   function buildHexString(id: integer; data: string): string;
   procedure closeLog;
   function logName: string;

implementation
uses
   sysUtils,
   commons;

var
   textLog: text;
   logOpen: boolean;

procedure logText(data: string);
begin
   if not logOpen then
   begin
      AssignFile(textLog, logName);
      rewrite(textLog);
      logOpen := true;
   end;
   writeln(textLog, data);
end;

function buildHexString(id: integer; data: string): string;
var
   i: integer;
begin
   result := intToHex(id, 1) + ' ' + intToHex(length(data), 1);
   for I := 1 to length(data) do
      result := result + ' ' + intToHex(byte(data[i]), 1);
end;

procedure closeLog;
begin
   if logOpen then
      closeFile(textLog);
end;

function logName: string;
begin
   result := IncludeTrailingPathDelimiter(GPRojectFolder) + 'TURBU log.txt';
end;

initialization
begin
   logOpen := false;
end;

finalization
begin
   closeLog;
end;

end.
