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
   sysUtils, db, rtti,
   turbu_classes, turbu_containers, turbu_defs, turbu_serialization,
   turbu_decl_utils,
   sg_defs;

type
   EScriptError = class(Exception);

   UploadRangeAttribute = class(TDBUploadAttribute)
      procedure upload(db: TDataset; field: TRttiField; instance: TObject); override;
      procedure download(db: TDataset; field: TRttiField; instance: TObject); override;
   end;

   TScriptRange = class(TRpgDatafile)
   private
      FDesignName: string;
      [UploadRange]
      FRange: TSgPoint;
      FUnit: string;
      FSignature: TScriptSignature;
   public
      constructor Create(name, designName: string; point: TSgPoint); reintroduce;

      property scriptUnitName: string read FUnit write FUnit;
      property range: TSgPoint read FRange write FRange;
      property designName: string read FDesignName;
      property signature: TScriptSignature read FSignature write FSignature;
   end;

   TScriptList = class(TRpgObjectList<TScriptRange>)
   private
      function findByName(name: string): TScriptRange;
   public
      property names[name: string]: TScriptRange read findByName;
   end;

implementation
uses
   strUtils;

{ TScriptRange }

constructor TScriptRange.Create(name, designName: string; point: TSgPoint);
var
   index, index2: integer;
begin
   inherited Create;
   index := pos(name, '.');
   assert(index > 0);
   FUnit := Copy(name, 1, index - 1);

   index2 := PosEx(name, ' ', index);
   Self.name := copy(name, index + 1, index2 - 1);
   FRange := point;
   FDesignName := designName;
   FSignature := ssNone;
end;

{ TScriptList }

function TScriptList.findByName(name: string): TScriptRange;
begin
   for result in self do
      if result.name = name then
         Exit;
   result := nil;
end;

{ TUploadRangeAttribute }

procedure UploadRangeAttribute.download(db: TDataset; field: TRttiField;
  instance: TObject);
var
   range: TScriptRange absolute instance;
begin
   assert(instance is TScriptRange);
   range.FRange.x := db.FieldByName('start').AsInteger;
   range.FRange.y := db.FieldByName('end').AsInteger;
end;

procedure UploadRangeAttribute.upload(db: TDataset; field: TRttiField;
  instance: TObject);
var
   range: TScriptRange absolute instance;
begin
   assert(instance is TScriptRange);
   db.FieldByName('start').AsInteger := range.FRange.x;
   db.FieldByName('end').AsInteger := range.FRange.y;
end;

end.
