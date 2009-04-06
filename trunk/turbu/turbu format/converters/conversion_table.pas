unit conversion_table;
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
   sysUtils;

type
   EConversionTableError = class(Exception);

   TConvertRecord = record
      before, after: integer;
   end;

   TConversionTable = class(TObject)
   private
      FTable: array of TConvertRecord;
      function getValue(x: integer): TConvertRecord; inline;
      function getLength: integer; inline;
   public
      destructor Destroy; override;
      procedure add(const before, after: integer);
      function indexOf(value: integer): integer;

      property len: integer read getLength;
      property list[x: integer]: TConvertREcord read getValue; default;
   end;

   TNameRecord = record
      name: string;
      id: integer;
   end;

   TNameTable = class(TObject)
   private
      FTable: array of TNameRecord;
      FDivMarkers: array of integer;
      function getValue(x: integer): TNameRecord; inline;
      function getLength: integer; inline;
   public
      destructor Destroy; override;
      procedure add(const name: string; id: integer);
      procedure newDivision;
      function indexOf(name: string; division: integer): integer;

      property len: integer read getLength;
      property list[x: integer]: TNameRecord read getValue; default;
   end;

implementation

{ TConversionTable }

procedure TConversionTable.add(const before, after: integer);
begin
   setLength(FTable, length(FTable) + 1);
   FTable[high(FTable)].before := before;
   FTable[high(FTable)].after := after;
end;

destructor TConversionTable.Destroy;
begin
   finalize(FTable);
   inherited;
end;

function TConversionTable.getLength: integer;
begin
   result := length(FTable);
end;

function TConversionTable.getValue(x: integer): TConvertRecord;
begin
   result := FTable[x];
end;

function TConversionTable.indexOf(value: integer): integer;
begin
   result := high(FTable);
   while (result >= 0) and (FTable[result].before <> value) do
      dec(result);
end;

{ TNameTable }

procedure TNameTable.add(const name: string; id: integer);
begin
   assert(length(FDivMarkers) > 0);
   setLength(FTable, length(FTable) + 1);
   FTable[high(FTable)].name := name;
   FTable[high(FTable)].id := id;
   inc(FDivMarkers[high(FDivMarkers)]);
end;

destructor TNameTable.Destroy;
begin
   finalize(FTable);
   inherited;
end;

function TNameTable.getLength: integer;
begin
   result := length(FTable);
end;

function TNameTable.getValue(x: integer): TNameRecord;
begin
   result := FTable[x];
end;

function TNameTable.indexOf(name: string; division: integer): integer;
var
   i: integer;
begin
   name := lowerCase(name);
   assert(division in [0..high(FDivMarkers)]);
   result := high(FTable);
   while (result >= 0) and (lowerCase(FTable[result].name) <> name) do
      dec(result);
   if result <> -1 then
      for I := 0 to division - 1 do
         dec(result, FDivMarkers[i]);
end;

procedure TNameTable.newDivision;
begin
   setLength(FDivMarkers, length(FDivMarkers) + 1);
end;

end.