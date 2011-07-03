unit shop_data;
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
   TStoreInventory = class(TObject)
   private
      FInventory: array of integer;
      FString: string;

      function getLength: integer;
      function getInventory(which: word): integer;
   public
      constructor Create(merchandise: string);

      property inventory[which: word]: integer read getInventory; default;
      property length: integer read getLength;
      property base: string read FString;
   end;

implementation
uses
   sysUtils, Classes,
   commons, script_engine, turbu_database;

{ TStoreInventory }

constructor TStoreInventory.Create(merchandise: string);
var
   i, j: integer;
   dummy: string;
   parser: TStringList;
begin
   inherited Create;
   FString := merchandise;
   i := 1;
   parser := TStringList.Create;
   try
      parser.Delimiter := ' ';
      parser.DelimitedText := merchandise;
      for dummy in parser do
      begin
         j := strToInt(dummy);
         if (dummy <> '') and (between(j, 0, GDatabase.items) = j) then
         begin
            setLength(FInventory, system.length(FInventory) + 1);
            FInventory[high(FInventory)] := strToInt(dummy);
         end;
      end;
   finally
      parser.Free;
   end;
end;

function TStoreInventory.getInventory(which: word): integer;
begin
   result := FInventory[which];
end;

function TStoreInventory.getLength: integer;
begin
   result := system.length(FInventory);
end;

end.
