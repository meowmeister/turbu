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

   EMissingPlugin = class(Exception);

   procedure addEngine(slot: TEngineStyle; value: TRpgMetadata; engine: IInterface);
   function requireEngine(slot: TEngineStyle; name: string; version: TVersion): TRpgMetadata;
   function retrieveEngine(slot: TEngineStyle; name: string; version: TVersion): IInterface; overload;
   function retrieveEngine(slot: TEngineStyle; value: TRpgMetadata): IInterface; overload;
   procedure cleanupEngines;

implementation
uses
   Generics.Collections;

type
   TEngineDict = class(TDictionary<TRpgMetadata, IInterface>);
   TEngineList = array[TEngineStyle] of TEngineDict;

var
   FEngineList: TEngineList;

procedure addEngine(slot: TEngineStyle; value: TRpgMetadata; engine: IInterface);
begin
   if not assigned(FEngineList[slot]) then
      FEngineList[slot] := TEngineDict.Create;
   FEngineList[slot].Add(value, engine);
end;

function requireEngine(slot: TEngineStyle; name: string; version: TVersion): TRpgMetadata;
resourcestring
  VERSION_TOO_LOW = 'This project requires the %s plugin at version %s or higher, but found version %s.';
  PLUGIN_NOT_FOUND = 'Unable to load %s, which is required for this project.';
var
   enumerator: TRpgMetadata;
begin
   result := nil;
   if assigned(FEngineList[slot]) then
      for enumerator in FEngineList[slot].Keys do
         if enumerator.name = name then
		 begin
            result := enumerator;
			break;
		 end;
   if assigned(result) then
   begin
      if result.version < version then
         raise EMissingPlugin.CreateFmt(VERSION_TOO_LOW, [name, version.name, result.version.name])
      else
         Exit;
   end
   else
      raise EMissingPlugin.CreateFmt(PLUGIN_NOT_FOUND, [name]);
end;

function retrieveEngine(slot: TEngineStyle; value: TRpgMetadata): IInterface;
begin
   result := FEngineList[slot][value];
end;

function retrieveEngine(slot: TEngineStyle; name: string; version: TVersion): IInterface;
begin
   result := retrieveEngine(slot, requireEngine(slot, name, version));
end;

procedure cleanupEngines;
var
   i: TEngineStyle;
begin
   for i := low(TEngineStyle) to high(TEngineStyle) do
      FreeAndNil(FEngineList[i]);
end;


initialization
finalization
   cleanupEngines;

end.
