unit turbu_unit_dictionary;
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
   classes, generics.Collections;

type
   TUnitDictionary = class(TObjectDictionary<string,TStringList>)
   private
      function GetItem(const Key: string): TStringList;
      procedure SetItem(const Key: string; const Value: TStringList);
   public
      procedure Add(const Key: string; const Value: TStringList);
      function ContainsKey(const Key: string): Boolean;

      property Items[const Key: string]: TStringList read GetItem write SetItem; default;
   end;

implementation
uses
   SysUtils;

{ TUnitDictionary }

procedure TUnitDictionary.Add(const Key: string; const Value: TStringList);
begin
   inherited Add(uppercase(key), value);
end;

function TUnitDictionary.ContainsKey(const Key: string): Boolean;
begin
   result := inherited ContainsKey(uppercase(key));
end;

function TUnitDictionary.GetItem(const Key: string): TStringList;
begin
   result := inherited Items[uppercase(key)];
end;

procedure TUnitDictionary.SetItem(const Key: string; const Value: TStringList);
begin
   inherited Items[uppercase(key)] := Value;
end;

end.
