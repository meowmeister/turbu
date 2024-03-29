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
   turbu_classes, turbu_defs, turbu_serialization;

type
   TScriptSignature = (ssNone, ssScriptEvent, ssDamageCalcEvent,
                       ssToHitEvent, ssCondOnTurnEvent, ssExpCalc,
                       ssSkillCheck);
{   TScriptEvent = procedure(character, party: TObject) of object;
   TDamageCalcEvent = function(character, target: TObject; var1, var2, var3, var4: integer; offensive: boolean): integer of object;
   TToHitEvent = function(character, target: TObject; effectiveness: integer; offensive: boolean): boolean of object;
   TCondOnTurnEvent = procedure(character, condition: TObject; var1, var2, var3, var4: integer) of object;
   TExpCalcEvent = function(level, var1, var2, var3, var4: integer): integer;}

   function signatureMatch(func: TRpgDecl): string;
   function GetSignature(const Event: string): TRpgDecl;

implementation
uses
   Generics.Collections,
   turbu_vartypes, turbu_containers;//, turbu_heroes;

type

   TSignatureDictionary = class(TObjectDictionary<string, TRpgDecl>);

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

function signatureMatch(func: TRpgDecl): string;
var
   iterator: TPair<string, TRpgDecl>;
begin
   for iterator in sigDict do
      if func.equals(iterator.Value) then
         Exit(iterator.Key);
   result := '';
end;

function GetSignature(const Event: string): TRpgDecl;
begin
   if not sigDict.TryGetValue(event, result) then
      result := nil;
end;

initialization
   sigDict := TSignatureDictionary.Create([doOwnsValues], 32);

finalization
   sigDict.Free;

end.
