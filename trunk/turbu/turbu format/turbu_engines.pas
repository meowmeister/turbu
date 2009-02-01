unit turbu_engines;
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
   SysUtils,
   turbu_plugin_interface, turbu_versioning;

type
   TEngineList = array[TEngineStyle] of array of TRpgMetadata;

   EMissingPlugin = class(Exception);

   procedure addEngine(slot: TEngineStyle; value: TRpgMetadata);
   function requireEngine(slot: TEngineStyle; name: string; version: TVersion): TRpgMetadata;
   procedure cleanupEngines;

implementation

var
   FEngineList: TEngineList;

procedure addEngine(slot: TEngineStyle; value: TRpgMetadata);
begin
   setLength(FEngineList[slot], length(FEngineList[slot]) + 1);
   FEngineList[slot][high(FEngineList[slot])] := value;
end;

function requireEngine(slot: TEngineStyle; name: string; version: TVersion): TRpgMetadata;
var
  i: integer;
begin
   i := high(FEngineList[slot]);
   while (i >= 0) and (TRpgMetadata(FEngineList[slot, i]).name <> name) do
      dec(i);
   if i >= 0 then
   begin
      result := FEngineList[slot, i] as TRpgMetadata;
      if result.version < version then
         raise EMissingPlugin.Create('This project requires the ' + name + 'plugin at version '
            + version.name + ' or higher, but found version ' + result.version.name + '.')
      else
         Exit;
   end
   else
      raise EMissingPlugin.Create('Unable to load ' + name + ', which is required for this project.');
end;

procedure cleanupEngines;
var
   i: TEngineStyle;
   j: integer;
begin
   for i := low(TEngineStyle) to high(TEngineStyle) do
      for j := 0 to high(FEngineList[i]) do
         FEngineList[i, j].free;
end;

end.
