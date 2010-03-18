unit turbu_decl_utils;
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
*****************************************************************************
*
* The GetMethodSignature function and its dependencies are adapted from code
* written by Hallvard Vassbotn. The original code can be found at
* http://hallvards.blogspot.com/2006/05/hack-10-getting-parameters-of.html
*****************************************************************************}

interface
uses
   types, TypInfo, DB,
   turbu_classes, turbu_heroes, turbu_defs;

type
   TScriptSignature = (ssNone = -1, ssScriptEvent, ssDamageCalcEvent,
                       ssToHitEvent, ssCondOnTurnEvent, ssExpCalc,
                       ssSkillCheck);
{   TScriptEvent = procedure(character, party: TObject) of object;
   TDamageCalcEvent = function(character, target: TObject; var1, var2, var3, var4: integer; offensive: boolean): integer of object;
   TToHitEvent = function(character, target: TObject; effectiveness: integer; offensive: boolean): boolean of object;
   TCondOnTurnEvent = procedure(character, condition: TObject; var1, var2, var3, var4: integer) of object;
   TExpCalcEvent = function(level, var1, var2, var3, var4: integer): integer;}

   TSkillBoolFunc = function(Character: TRpgHero; Level, unused2, unused3, unused4: integer): boolean of object;
   TSkillNumFunc = function(character: TRpgHero; int1, int2, int3, int4: integer): integer of object;
   TSkillDualNumFunc = function(character: TRpgHero; int1, int2, int3, int4: integer): TPoint of object;

   TScriptRecord = class(TObject)
   private
      FName: string;
      FDesignName: string;
      FStrings: T4StrArray;
      FMethod: TMethod;
      FSignature: TScriptSignature;
      function getMethod: TMethod;
      procedure setName(const Value: string);
   public
      constructor Create(decl: TRpgDecl; script: TMethod);
      procedure upload(db: TDataSet);
      procedure update(db: TDataSet);

      property name: string read FName write setName;
      property designName: string read FDesignName write FDesignName;
      property baseMethod: TMethod read getMethod;
   end;

   function signatureMatch(func: TRpgDecl): TScriptSignature; overload;
   function GetSignature(Event: PTypeInfo): TRpgDecl;

implementation
uses
   Generics.Collections,
   turbu_vartypes, turbu_containers;

type

   {This has to use a PTypeInfo record instead of the function type itself,
   since the function type doesn't really exist as an independent entity. The
   TypeInfo routine is compiler magic, not a real function}
   TSignatureDictionary = class(TObjectDictionary<PTypeInfo, TRpgDecl>)
   private
      function GetItem(const Key: PTypeInfo): TRpgDecl;
   public
      property Item[const Key: PTypeInfo]: TRpgDecl read GetItem; default;
   end;

var
   sigDict: TSignatureDictionary;

function PackedShortString(Value: PShortstring; var NextField{: Pointer}): PShortString; overload;
begin
   Result := Value;
   PShortString(NextField) := Value;
   Inc(PAnsiChar(NextField), SizeOf(Result^[0]) + Length(Result^));
end;

function PackedShortString(var NextField{: Pointer}): PShortString; overload;
begin
   Result := PShortString(NextField);
   Inc(PAnsiChar(NextField), SizeOf(Result^[0]) + Length(Result^));
end;

{$T-}
function GetMethodSignature(Event: PTypeInfo): TRpgDecl;
type
   PParamListRecord = ^TParamListRecord;
   TParamListRecord = packed record
      Flags: TParamFlags;
      ParamName: {packed} ShortString; // Really string[Length(ParamName)]
      TypeName:  {packed} ShortString; // Really string[Length(TypeName)]
   end;
var
   EventData: PTypeData;
   i: integer;
   MethodParam: TNameType;
   ParamListRecord: PParamListRecord;
begin
   assert(Event.Kind = tkMethod);
   eventData := GetTypeData(event);
   result := TRpgDecl.Create(string(event.Name), '');
   ParamListRecord := @EventData.ParamList;
   for i := 0 to EventData.ParamCount - 1 do
   begin
      MethodParam.Flags := ParamListRecord.Flags;
      MethodParam.name := string(PackedShortString( @ParamListRecord.ParamName, ParamListRecord)^);
      MethodParam.typeVar := lookupType(string(PackedShortString(ParamListRecord)^));
      result.params.Add(MethodParam);
   end;
   result.retval := lookupType(string(PackedShortString(ParamListRecord)^));
end;
{$T+}

function signatureMatch(func: TRpgDecl; signature: TScriptSignature): boolean; overload;
begin
   case signature of
      ssExpCalc: result := func.equals(sigDict[TypeInfo(TExpCalcEvent)]);
      ssSkillCheck: result := func.equals(sigDict[TypeInfo(TSkillBoolFunc)]) or
                              func.equals(sigDict[TypeInfo(TSkillNumFunc)]) or
                              func.equals(sigDict[TypeInfo(TSkillDualNumFunc)]);
      else result := false;
   end;
end;

function signatureMatch(func: TRpgDecl): TScriptSignature; overload;
var
   iterator: TScriptSignature;
begin
   result := ssNone;
   for iterator := high(TScriptSignature) downto low(TScriptSignature) do
      if (ord(iterator) >= 0) and (signatureMatch(func, iterator)) then
         Exit(iterator);
end;

function GetSignature(Event: PTypeInfo): TRpgDecl;
begin
   result := sigDict[Event];
end;

{ TSignatureDictionary }

function TSignatureDictionary.GetItem(const Key: PTypeInfo): TRpgDecl;
begin
   if not self.TryGetValue(key, result) then
   begin
      result := GetMethodSignature(key);
      self.Add(key, result);
   end;
end;

{ TScriptRecord }

constructor TScriptRecord.Create(decl: TRpgDecl; script: TMethod);
var
   i: integer;
begin
   FName := decl.name;
   FDesignName := decl.designName;
   FMethod := script;
   if decl.fourInts then
      for I := 1 to 4 do
         FStrings[i] := decl.params[i].name;
   FSignature := signatureMatch(decl);
end;

function TScriptRecord.getMethod: TMethod;
begin
   result := FMethod;
end;

procedure TScriptRecord.setName(const Value: string);
begin
  FName := Value;
end;

procedure TScriptRecord.update(db: TDataSet);
begin
   db.FieldByName('name').AsString := name;
   db.FieldByName('designName').AsString := designName;
   db.FieldByName('address').AsInteger := integer(Self);
   db.FieldByName('baseMethod').asPSMethod := TMethod(FMethod);
end;

procedure TScriptRecord.upload(db: TDataSet);
begin
   db.Append;
   self.update(db);
end;

initialization
   sigDict := TSignatureDictionary.Create([doOwnsValues], 32);

finalization
   sigDict.Free;

end.
