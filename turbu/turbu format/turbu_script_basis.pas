unit turbu_script_basis;
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
   sysUtils, db,
   commons, turbu_classes, turbu_containers;

type
   EScriptError = class(Exception);

   TScriptRange = class(TRpgDatafile)
   private
      FRange: TRpgPoint;
      FUnit: string;
   public
      constructor Create(name: string; point: TRpgPoint);
      procedure upload(db: TDataSet); override;

      property scriptUnitName: string read FUnit write FUnit;
      property range: TRpgPoint read FRange write FRange;
   end;

   TScriptList = class(TRpgObjectList<TScriptRange>)
   private
      function findByName(name: string): TScriptRange;
   public
      property names[name: string]: TScriptRange read findByName;
   end;

implementation
uses
   strtok;

{ TScriptRange }

constructor TScriptRange.Create(name: string; point: TRpgPoint);
var
   index: integer;
begin
   inherited Create;
   index := 1;
   FUnit := strtok.GetNextToken(name, '.', index);
   Self.name := GetNextToken(name, ' ', index);
   FRange := point;
end;

procedure TScriptRange.upload(db: TDataSet);
begin
   inherited upload(db);
   db.FieldByName('unit').AsString := FUnit;
   db.FieldByName('start').AsInteger := FRange.x;
   db.FieldByName('end').AsInteger := FRange.y;
end;

{ TScriptList }

function TScriptList.findByName(name: string): TScriptRange;
begin
   for result in self do
      if result.name = name then
         Exit;
   result := nil;
end;

end.
