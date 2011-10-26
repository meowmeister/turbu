unit turbu_vartypes;
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

   function lookupType(index: integer): string; overload;
   function lookupType(index: string): integer; overload;
   function registerType(name: string): integer;
   function getTypeList: TStringList;

implementation
uses
   sysUtils, Generics.collections,
   turbu_constants;

type
   TTranslator = TDictionary<string, string>;

var
   typeList: TStringList;
   translator: TTranslator;

function lookupType(index: integer): string;
begin
   if (index < typeList.Count) and (index >= 0) then
      result := typeList[index]
   else result := '';
end;

function lookupType(index: string): integer;
var
   newval: string;
begin
   if translator.TryGetValue(lowercase(index), newval) then
      result := typeList.IndexOf(newval)
   else result := typeList.IndexOf(index);
end;

function registerType(name: string): integer;
begin
   result := typeList.Add(name);
end;

function getTypeList: TStringList;
begin
   result := typeList;
end;

var
   i: integer;

initialization
begin
   typeList := TStringList.Create;
   typeList.CaseSensitive := false;
   for i := low(TYPENAMES) to high(TYPENAMES) do
      assert(registerType(TYPENAMES[i]) = i);

   translator := TTranslator.Create;
   translator.Add('longint', 'integer');
   translator.Add('longword', 'cardinal');
end;

finalization
begin
   typeList.Free;
   translator.Free;
end;

end.
