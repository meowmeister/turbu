(*
* Copyright (c) 2008-2009, Ciobanu Alexandru
* All rights reserved.
*
* Redistribution and use in source and binary forms, with or without
* modification, are permitted provided that the following conditions are met:
*     * Redistributions of source code must retain the above copyright
*       notice, this list of conditions and the following disclaimer.
*     * Redistributions in binary form must reproduce the above copyright
*       notice, this list of conditions and the following disclaimer in the
*       documentation and/or other materials provided with the distribution.
*     * Neither the name of the <organization> nor the
*       names of its contributors may be used to endorse or promote products
*       derived from this software without specific prior written permission.
*
* THIS SOFTWARE IS PROVIDED BY THE AUTHOR ''AS IS'' AND ANY
* EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
* WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
* DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
* DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
* (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
* LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
* ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
* (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
* SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)

{$I ../DeHL.Defines.inc}
unit DeHL.KeyValuePair;
interface
uses
  SysUtils,
  DeHL.Base,
  DeHL.Exceptions,
  DeHL.StrConsts,
  DeHL.Arrays,
  DeHL.Types,
  DeHL.Serialization;

type
  { Key value pair }
  TKeyValuePair<TKey, TValue> = record
  private
    FKey: TKey;
    FValue: TValue;
{$HINTS OFF}
    class constructor Create();
    class destructor Destroy();
{$HINTS ON}
  public
    { Constructors }
    constructor Create(const AKey: TKey; const AValue: TValue); overload;
    constructor Create(const APair: TKeyValuePair<TKey, TValue>); overload;

    { Properties }
    property Key: TKey read FKey;
    property Value: TValue read FValue;
  end;

  { Nullable Support }
  TKeyValuePairType<TKey, TValue> = class(TRecordType<TKeyValuePair<TKey, TValue>>)
  private
    FKeyType: IType<TKey>;
    FValueType: IType<TValue>;
    FTypeManagement: TTypeManagement;

  protected
    { Serialization }
    procedure DoSerialize(const AInfo: TValueInfo; const AValue: TKeyValuePair<TKey, TValue>; const AContext: ISerializationContext); override;
    procedure DoDeserialize(const AInfo: TValueInfo; out AValue: TKeyValuePair<TKey, TValue>; const AContext: IDeserializationContext); override;

  public
    { Constructors }
    constructor Create(); overload; override;
    constructor Create(const AKeyType: IType<TKey>; const AValueType: IType<TValue>); reintroduce; overload;

    { Comparator }
    function Compare(const AValue1, AValue2: TKeyValuePair<TKey, TValue>): Integer; override;

    { Equality Comparator }
    function AreEqual(const AValue1, AValue2: TKeyValuePair<TKey, TValue>): Boolean; override;

    { Hash code provider }
    function GenerateHashCode(const AValue: TKeyValuePair<TKey, TValue>): Integer; override;

    { Get String representation }
    function GetString(const AValue: TKeyValuePair<TKey, TValue>): String; override;

    { Type management }
    function Management(): TTypeManagement; override;

    { Cleanup / management }
    procedure Cleanup(var AValue: TKeyValuePair<TKey, TValue>); override;

    { Variant Conversion }
    function TryConvertToVariant(const AValue: TKeyValuePair<TKey, TValue>; out ORes: Variant): Boolean; override;
    function TryConvertFromVariant(const AValue: Variant; out ORes: TKeyValuePair<TKey, TValue>): Boolean; override;
  end;


implementation
uses Variants;

{ RKeyValuePair<TKey, TValue> }

constructor TKeyValuePair<TKey, TValue>.Create(const AKey: TKey; const AValue: TValue);
begin
  { Copy values }
  FKey := AKey;
  FValue := AValue;
end;

constructor TKeyValuePair<TKey, TValue>.Create(const APair: TKeyValuePair<TKey, TValue>);
begin
  { Copy values }
  FKey := APair.FKey;
  FValue := APair.FValue;
end;

class constructor TKeyValuePair<TKey, TValue>.Create;
begin
  { Register custom type }
  if not TType<TKeyValuePair<TKey, TValue>>.IsRegistered then
    TType<TKeyValuePair<TKey, TValue>>.Register(TKeyValuePairType<TKey, TValue>);
end;

class destructor TKeyValuePair<TKey, TValue>.Destroy;
begin
  { Unregister custom type }
  if TType<TKeyValuePair<TKey, TValue>>.IsRegistered then
    TType<TKeyValuePair<TKey, TValue>>.Unregister();
end;

{ TKeyValuePairType<TKey, TValue> }

function TKeyValuePairType<TKey, TValue>.AreEqual(const AValue1, AValue2: TKeyValuePair<TKey, TValue>): Boolean;
begin
  { Check for both key and value }
  Result := FKeyType.AreEqual(AValue1.FKey, AValue2.FKey) and
            FValueType.AreEqual(AValue1.FValue, AValue2.FValue);
end;

procedure TKeyValuePairType<TKey, TValue>.Cleanup(var AValue: TKeyValuePair<TKey, TValue>);
begin
  if Management = tmManual then
  begin
    if FKeyType.Management = tmManual then
      FKeyType.Cleanup(AValue.FKey);

    if FValueType.Management = tmManual then
      FValueType.Cleanup(AValue.FValue);
  end;
end;

function TKeyValuePairType<TKey, TValue>.Compare(const AValue1, AValue2: TKeyValuePair<TKey, TValue>): Integer;
begin
  { First compare the keys and only then the values }
  Result := FKeyType.Compare(AValue1.FKey, AValue2.FKey);

  if Result = 0 then
    Result := FValueType.Compare(AValue1.FValue, AValue2.FValue);
end;

constructor TKeyValuePairType<TKey, TValue>.Create;
begin
  { Call upper constructor }
  Create(TType<TKey>.Default, TType<TValue>.Default);
end;

constructor TKeyValuePairType<TKey, TValue>.Create(const AKeyType: IType<TKey>; const AValueType: IType<TValue>);
begin
  inherited Create();

  if AKeyType = nil then
    ExceptionHelper.Throw_ArgumentNilError('AKeyType');

  if AValueType = nil then
    ExceptionHelper.Throw_ArgumentNilError('AValueType');

  { Obtain the type classes }
  FKeyType := AKeyType;
  FValueType := AValueType;

  { If at least one is manual, declare as manual }
  if (FKeyType.Management() = tmManual) or (FValueType.Management() = tmManual) then
    FTypeManagement := tmManual
  else if (FKeyType.Management() = tmCompiler) or (FValueType.Management() = tmCompiler) then
    FTypeManagement := tmCompiler
  else
    FTypeManagement := tmNone;
end;

procedure TKeyValuePairType<TKey, TValue>.DoDeserialize(const AInfo: TValueInfo;
  out AValue: TKeyValuePair<TKey, TValue>;
  const AContext: IDeserializationContext);
begin
  AContext.ExpectRecordType(AInfo);

  { Serialize the key and value in the block }
  FKeyType.Deserialize(TValueInfo.Create(SSerKey), AValue.FKey, AContext);
  FValueType.Deserialize(TValueInfo.Create(SSerValue), AValue.FValue, AContext);

  AContext.EndComplexType();
end;

procedure TKeyValuePairType<TKey, TValue>.DoSerialize(const AInfo: TValueInfo;
  const AValue: TKeyValuePair<TKey, TValue>;
  const AContext: ISerializationContext);
begin
  AContext.StartRecordType(AInfo);

  { Serialize the key and value in the block }
  FKeyType.Serialize(TValueInfo.Create(SSerKey), AValue.FKey, AContext);
  FValueType.Serialize(TValueInfo.Create(SSerValue), AValue.FValue, AContext);

  AContext.EndComplexType();
end;

function TKeyValuePairType<TKey, TValue>.GenerateHashCode(const AValue: TKeyValuePair<TKey, TValue>): Integer;
var
  LArray: array[0 .. 1] of Integer;
begin
  LArray[0] := FKeyType.GenerateHashCode(AValue.FKey);
  LArray[1] := FValueType.GenerateHashCode(AValue.FValue);

  Result := BinaryHash(@LArray[0], SizeOf(LArray));
end;

function TKeyValuePairType<TKey, TValue>.GetString(const AValue: TKeyValuePair<TKey, TValue>): String;
begin
  { Combine the strings of both key and value into one }
  Result := Format(SPair, [FKeyType.GetString(AValue.FKey), FValueType.GetString(AValue.FValue)]);
end;

function TKeyValuePairType<TKey, TValue>.Management: TTypeManagement;
begin
  Result := FTypeManagement;
end;

function TKeyValuePairType<TKey, TValue>.TryConvertFromVariant(const AValue: Variant; out ORes: TKeyValuePair<TKey, TValue>): Boolean;
var
  LVarType: TVarType;
begin
  LVarType := VarType(AValue);

  if (LVarType = (varArray or varVariant)) and
     (VarArrayDimCount(AValue) = 1) and
     (VarArrayHighBound(AValue, 1) = 1) and
     (VarArrayLowBound(AValue, 1) = 0) then
  begin
    { This is a variant array, let's crack it open }
    Result := FKeyType.TryConvertFromVariant(AValue[0], ORes.FKey) and
      FValueType.TryConvertFromVariant(AValue[1], ORes.FValue);
  end else
    Result := false;
end;

function TKeyValuePairType<TKey, TValue>.TryConvertToVariant(const AValue: TKeyValuePair<TKey, TValue>; out ORes: Variant): Boolean;
var
  LKey, LValue: Variant;
begin
  if FKeyType.TryConvertToVariant(AValue.FKey, LKey) and
     FValueType.TryConvertToVariant(AValue.FValue, LValue) then
  begin
    { Create the variant array }
    ORes := VarArrayOf([LKey, LValue]);
    Result := true;
  end
    else Result := false;
end;

end.
