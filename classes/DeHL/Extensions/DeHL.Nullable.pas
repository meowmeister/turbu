(*
* Copyright (c) 2009-2010, Ciobanu Alexandru
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
unit DeHL.Nullable;
interface
uses SysUtils,
     DeHL.Base,
     DeHL.StrConsts,
     DeHL.Exceptions,
     DeHL.Serialization,
     DeHL.Types;

type
  { Nullable type }
  Nullable<T> = record
{$HINTS OFF}
  private
    class var __Marker: IInterface;

    class constructor Create();
    class destructor Destroy();
{$HINTS ON}

  private
    FMarker: IInterface;
    FValue: T;

    function GetIsNull: Boolean; inline;
    function GetValue: T; inline;
    procedure SetValue(const Value: T); inline;
    function GetValueOrDefault: T; inline;
  public
    { Constuctors }
    constructor Create(const AValue: T);

    { Nulling }
    property IsNull: Boolean read GetIsNull;
    property Value: T read GetValue write SetValue;
    property ValueOrDefault: T read GetValueOrDefault;

    procedure MakeNull(); inline;

    { Value }
    class operator Implicit(const AValue: Nullable<T>): T; inline;
    class operator Implicit(const AValue: T): Nullable<T>; inline;
  end;

  { Nullable Support }
  TNullableType<T> = class(TMagicType<Nullable<T>>)
  private
    FType: IType<T>;

  protected
    { Serialization }
    procedure DoSerialize(const AInfo: TValueInfo; const AValue: Nullable<T>; const AContext: ISerializationContext); override;
    procedure DoDeserialize(const AInfo: TValueInfo; out AValue: Nullable<T>; const AContext: IDeserializationContext); override;

  public
    { Constructors }
    constructor Create(); overload; override;
    constructor Create(const AType: IType<T>); reintroduce; overload;

    { Comparator }
    function Compare(const AValue1, AValue2: Nullable<T>): Integer; override;

    { Equality Comparator }
    function AreEqual(const AValue1, AValue2: Nullable<T>): Boolean; override;

    { Hash code provider }
    function GenerateHashCode(const AValue: Nullable<T>): Integer; override;

    { Value Cleanup/Management }
    function Management(): TTypeManagement; override;
    procedure Cleanup(var AValue: Nullable<T>); override;

    { Get String representation }
    function GetString(const AValue: Nullable<T>): String; override;

    { Variant Conversion }
    function TryConvertToVariant(const AValue: Nullable<T>; out ORes: Variant): Boolean; override;
    function TryConvertFromVariant(const AValue: Variant; out ORes: Nullable<T>): Boolean; override;
  end;

implementation

{ Nullable<T> }

constructor Nullable<T>.Create(const AValue: T);
begin
  SetValue(AValue);
end;

class constructor Nullable<T>.Create;
begin
  __Marker := TInterfacedObject.Create();

  { Register custom type }
  if not TType<Nullable<T>>.IsRegistered then
    TType<Nullable<T>>.Register(TNullableType<T>);
end;

class destructor Nullable<T>.Destroy;
begin
  __Marker := nil;

  { Unregister the custom type }
  if not TType<Nullable<T>>.IsRegistered then
    TType<Nullable<T>>.Unregister();
end;

function Nullable<T>.GetIsNull: Boolean;
begin
  Result := (FMarker = nil);
end;

function Nullable<T>.GetValue: T;
begin
  if FMarker = nil then
    ExceptionHelper.Throw_NullValueRequested();

  Result := FValue;
end;

function Nullable<T>.GetValueOrDefault: T;
begin
  { So simple }
  if FMarker = nil then
    Result := default(T)
  else
    Result := FValue;
end;

class operator Nullable<T>.Implicit(const AValue: T): Nullable<T>;
begin
  { Initialize a value }
  Result := Nullable<T>.Create(AValue);
end;

class operator Nullable<T>.Implicit(const AValue: Nullable<T>): T;
begin
  Result := AValue.GetValue();
end;

procedure Nullable<T>.MakeNull;
begin
  { Kill value }
  FValue := Default(T);
  FMarker := nil;
end;

procedure Nullable<T>.SetValue(const Value: T);
begin
  FMarker := __Marker;
  FValue := Value;
end;

{ TNullableType<T> }

function TNullableType<T>.AreEqual(const AValue1, AValue2: Nullable<T>): Boolean;
begin
  { Use the enclosed type }
  Result := FType.AreEqual(AValue1.ValueOrDefault, AValue2.ValueOrDefault);
end;

procedure TNullableType<T>.Cleanup(var AValue: Nullable<T>);
begin
  { Use the enclosed type }
  if not AValue.IsNull then
    FType.Cleanup(AValue.FValue);
end;

function TNullableType<T>.Compare(const AValue1, AValue2: Nullable<T>): Integer;
begin
  { Use the enclosed type }
  Result := FType.Compare(AValue1.ValueOrDefault, AValue2.ValueOrDefault);
end;

constructor TNullableType<T>.Create(const AType: IType<T>);
begin
  inherited Create();

  if AType = nil then
    ExceptionHelper.Throw_ArgumentNilError('AType');

  FType := AType;
end;

constructor TNullableType<T>.Create;
begin
  inherited;

  { Obtain the type }
  FType := TType<T>.Default;
end;

function TNullableType<T>.GenerateHashCode(const AValue: Nullable<T>): Integer;
begin
  { Use the enclosed type }
  Result := FType.GenerateHashCode(AValue.ValueOrDefault);
end;

function TNullableType<T>.GetString(const AValue: Nullable<T>): String;
begin
  { Use the enclosed type }
  Result := FType.GetString(AValue.ValueOrDefault);
end;

function TNullableType<T>.Management: TTypeManagement;
begin
  Result := FType.Management;
end;

function TNullableType<T>.TryConvertFromVariant(const AValue: Variant; out ORes: Nullable<T>): Boolean;
var
  LV: T;
begin
  { Use the enclosed type }
  Result := FType.TryConvertFromVariant(AValue, LV);

  if Result then
    ORes.Value := LV;
end;

function TNullableType<T>.TryConvertToVariant(const AValue: Nullable<T>; out ORes: Variant): Boolean;
begin
  { Use the enclosed type }
  Result := FType.TryConvertToVariant(AValue.ValueOrDefault, ORes);
end;

procedure TNullableType<T>.DoDeserialize(const AInfo: TValueInfo; out AValue: Nullable<T>; const AContext: IDeserializationContext);
var
  LIsDefined: Boolean;
begin
  { Pass over }
  AContext.ExpectRecordType(AInfo);

  { Get contents }
  AContext.GetValue(TValueInfo.Create(SIsDefined), LIsDefined);
  FType.Deserialize(TValueInfo.Create(SSerValue), AValue.FValue, AContext);

  if LIsDefined then
    AValue.FMarker := __Marker;

  AContext.EndComplexType();
end;

procedure TNullableType<T>.DoSerialize(const AInfo: TValueInfo; const AValue: Nullable<T>; const AContext: ISerializationContext);
begin
  { Pass over }
  AContext.StartRecordType(AInfo);

  AContext.AddValue(TValueInfo.Create(SIsDefined), not AValue.IsNull);
  FType.Serialize(TValueInfo.Create(SSerValue), AValue.ValueOrDefault, AContext);

  AContext.EndComplexType();
end;

end.
