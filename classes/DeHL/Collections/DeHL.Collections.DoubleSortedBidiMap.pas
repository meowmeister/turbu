(*
* Copyright (c) 2009, Ciobanu Alexandru
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
unit DeHL.Collections.DoubleSortedBidiMap;
interface
uses
  SysUtils,
  DeHL.Base,
  DeHL.Exceptions,
  DeHL.Types,
  DeHL.StrConsts,
  DeHL.Arrays,
  DeHL.Serialization,
  DeHl.KeyValuePair,
  DeHL.Collections.Base,
  DeHL.Collections.Abstract,
  DeHL.Collections.DoubleSortedDistinctMultiMap;

type
  { Multi-Map based on a dictionary and a list }
  TDoubleSortedBidiMap<TKey, TValue> = class(TAbstractBidiMap<TKey, TValue>)
  private
    FAscKeys, FAscValues: Boolean;

  protected
    { Override to provide map implemenatations }
    function CreateKeyMap(const AKeyType: IType<TKey>;
      const AValueType: IType<TValue>): IDistinctMultiMap<TKey, TValue>; override;

    function CreateValueMap(const AValueType: IType<TValue>;
      const AKeyType: IType<TKey>): IDistinctMultiMap<TValue, TKey>; override;

    { Serialization overrides }
    procedure StartSerializing(const AData: TSerializationData); override;
    procedure StartDeserializing(const AData: TDeserializationData); override;
    procedure DeserializePair(const AKey: TKey; const AValue: TValue); override;
  public
    { Constructors }
    constructor Create(const AscendingKeys: Boolean = true; const AscendingValues: Boolean = true); overload;
    constructor Create(const AEnumerable: IEnumerable<TKeyValuePair<TKey,TValue>>;
          const AscendingKeys: Boolean = true; const AscendingValues: Boolean = true); overload;
    constructor Create(const AArray: array of TKeyValuePair<TKey,TValue>;
          const AscendingKeys: Boolean = true; const AscendingValues: Boolean = true); overload;
    constructor Create(const AArray: TDynamicArray<TKeyValuePair<TKey, TValue>>;
          const AscendingKeys: Boolean = true; const AscendingValues: Boolean = true); overload;
    constructor Create(const AArray: TFixedArray<TKeyValuePair<TKey, TValue>>;
          const AscendingKeys: Boolean = true; const AscendingValues: Boolean = true); overload;

    constructor Create(const AKeyType: IType<TKey>; const AValueType: IType<TValue>;
          const AscendingKeys: Boolean = true; const AscendingValues: Boolean = true); overload;
    constructor Create(const AKeyType: IType<TKey>; const AValueType: IType<TValue>;
          const AEnumerable: IEnumerable<TKeyValuePair<TKey,TValue>>; const AscendingKeys: Boolean = true;
          const AscendingValues: Boolean = true); overload;
    constructor Create(const AKeyType: IType<TKey>; const AValueType: IType<TValue>;
          const AArray: array of TKeyValuePair<TKey,TValue>; const AscendingKeys: Boolean = true;
          const AscendingValues: Boolean = true); overload;
    constructor Create(const AKeyType: IType<TKey>; const AValueType: IType<TValue>;
          const AArray: TDynamicArray<TKeyValuePair<TKey,TValue>>; const AscendingKeys: Boolean = true;
          const AscendingValues: Boolean = true); overload;
    constructor Create(const AKeyType: IType<TKey>; const AValueType: IType<TValue>;
          const AArray: TFixedArray<TKeyValuePair<TKey,TValue>>; const AscendingKeys: Boolean = true;
          const AscendingValues: Boolean = true); overload;

    { Enex - Associative collection }
    function MaxKey(): TKey; override;
    function MinKey(): TKey; override;
  end;

  { The object variant }
  TObjectDoubleSortedBidiMap<TKey, TValue: class> = class sealed(TDoubleSortedBidiMap<TKey, TValue>)
  private
    FKeyWrapperType: TObjectWrapperType<TKey>;
    FValueWrapperType: TObjectWrapperType<TValue>;

    { Getters/Setters for OwnsKeys }
    function GetOwnsKeys: Boolean;
    procedure SetOwnsKeys(const Value: Boolean);

    { Getters/Setters for OwnsValues }
    function GetOwnsValues: Boolean;
    procedure SetOwnsValues(const Value: Boolean);

  protected
    { Override in descendants to support proper stuff }
    procedure InstallTypes(const AKeyType: IType<TKey>; const AValueType: IType<TValue>); override;

  public
    { Object owning }
    property OwnsKeys: Boolean read GetOwnsKeys write SetOwnsKeys;
    property OwnsValues: Boolean read GetOwnsValues write SetOwnsValues;
  end;

implementation

{ TDoubleSortedBidiMap<TKey, TValue> }

constructor TDoubleSortedBidiMap<TKey, TValue>.Create(
  const AArray: TDynamicArray<TKeyValuePair<TKey, TValue>>; const AscendingKeys,
  AscendingValues: Boolean);
begin
  { Do da dew and continue! }
  FAscKeys := AscendingKeys;
  FAscValues := AscendingValues;

  inherited Create(AArray);
end;

constructor TDoubleSortedBidiMap<TKey, TValue>.Create(
  const AArray: TFixedArray<TKeyValuePair<TKey, TValue>>; const AscendingKeys,
  AscendingValues: Boolean);
begin
  { Do da dew and continue! }
  FAscKeys := AscendingKeys;
  FAscValues := AscendingValues;

  inherited Create(AArray);
end;

constructor TDoubleSortedBidiMap<TKey, TValue>.Create(
  const AArray: array of TKeyValuePair<TKey, TValue>; const AscendingKeys,
  AscendingValues: Boolean);
begin
  { Do da dew and continue! }
  FAscKeys := AscendingKeys;
  FAscValues := AscendingValues;

  inherited Create(AArray);
end;

constructor TDoubleSortedBidiMap<TKey, TValue>.Create(const AscendingKeys, AscendingValues: Boolean);
begin
  { Do da dew and continue! }
  FAscKeys := AscendingKeys;
  FAscValues := AscendingValues;

  inherited Create();
end;

constructor TDoubleSortedBidiMap<TKey, TValue>.Create(
  const AEnumerable: IEnumerable<TKeyValuePair<TKey, TValue>>;
  const AscendingKeys, AscendingValues: Boolean);
begin
  { Do da dew and continue! }
  FAscKeys := AscendingKeys;
  FAscValues := AscendingValues;

  inherited Create(AEnumerable);
end;

constructor TDoubleSortedBidiMap<TKey, TValue>.Create(
  const AKeyType: IType<TKey>; const AValueType: IType<TValue>;
  const AArray: TDynamicArray<TKeyValuePair<TKey, TValue>>; const AscendingKeys,
  AscendingValues: Boolean);
begin
  { Do da dew and continue! }
  FAscKeys := AscendingKeys;
  FAscValues := AscendingValues;

  inherited Create(AKeyType, AValueType, AArray);
end;

constructor TDoubleSortedBidiMap<TKey, TValue>.Create(
  const AKeyType: IType<TKey>; const AValueType: IType<TValue>;
  const AArray: TFixedArray<TKeyValuePair<TKey, TValue>>; const AscendingKeys,
  AscendingValues: Boolean);
begin
  { Do da dew and continue! }
  FAscKeys := AscendingKeys;
  FAscValues := AscendingValues;

  inherited Create(AKeyType, AValueType, AArray);
end;

constructor TDoubleSortedBidiMap<TKey, TValue>.Create(
  const AKeyType: IType<TKey>; const AValueType: IType<TValue>;
  const AArray: array of TKeyValuePair<TKey, TValue>; const AscendingKeys,
  AscendingValues: Boolean);
begin
  { Do da dew and continue! }
  FAscKeys := AscendingKeys;
  FAscValues := AscendingValues;

  inherited Create(AKeyType, AValueType, AArray);
end;

constructor TDoubleSortedBidiMap<TKey, TValue>.Create(
  const AKeyType: IType<TKey>; const AValueType: IType<TValue>;
  const AscendingKeys, AscendingValues: Boolean);
begin
  { Do da dew and continue! }
  FAscKeys := AscendingKeys;
  FAscValues := AscendingValues;

  inherited Create(AKeyType, AValueType);
end;

constructor TDoubleSortedBidiMap<TKey, TValue>.Create(
  const AKeyType: IType<TKey>; const AValueType: IType<TValue>;
  const AEnumerable: IEnumerable<TKeyValuePair<TKey, TValue>>;
  const AscendingKeys, AscendingValues: Boolean);
begin
  { Do da dew and continue! }
  FAscKeys := AscendingKeys;
  FAscValues := AscendingValues;

  inherited Create(AKeyType, AValueType, AEnumerable);
end;

function TDoubleSortedBidiMap<TKey, TValue>.CreateKeyMap(const AKeyType: IType<TKey>;
  const AValueType: IType<TValue>): IDistinctMultiMap<TKey, TValue>;
begin
  { Use a double sorted map }
  Result := TDoubleSortedDistinctMultiMap<TKey, TValue>.Create(AKeyType, AValueType, FAscKeys, FAscValues);
end;

function TDoubleSortedBidiMap<TKey, TValue>.CreateValueMap(const AValueType: IType<TValue>;
  const AKeyType: IType<TKey>): IDistinctMultiMap<TValue, TKey>;
begin
  { Use a double sorted map }
  Result := TDoubleSortedDistinctMultiMap<TValue, TKey>.Create(AValueType, AKeyType, FAscKeys, FAscValues);
end;

procedure TDoubleSortedBidiMap<TKey, TValue>.DeserializePair(const AKey: TKey; const AValue: TValue);
begin
  { Simple as that }
  Add(AKEy, AValue);
end;

function TDoubleSortedBidiMap<TKey, TValue>.MaxKey: TKey;
begin
  Result := ByKeyMap.MaxKey;
end;

function TDoubleSortedBidiMap<TKey, TValue>.MinKey: TKey;
begin
  Result := ByKeyMap.MinKey;
end;

procedure TDoubleSortedBidiMap<TKey, TValue>.StartDeserializing(const AData: TDeserializationData);
var
  LAscKeys, LAscValues: Boolean;
begin
  { Try to obtain the ascending sign }
  AData.GetValue(SSerAscendingKeys, LAscKeys);
  AData.GetValue(SSerAscendingValues, LAscValues);

  { Call the constructor in this instance to initialize myself first }
  Create(LAscKeys, LAscValues);
end;

procedure TDoubleSortedBidiMap<TKey, TValue>.StartSerializing(const AData: TSerializationData);
begin
  { Write the ascending sign }
  AData.AddValue(SSerAscendingKeys, FAscKeys);
  AData.AddValue(SSerAscendingValues, FAscValues);
end;

{ TObjectDoubleSortedBidiMap<TKey, TValue> }

procedure TObjectDoubleSortedBidiMap<TKey, TValue>.InstallTypes(const AKeyType: IType<TKey>; const AValueType: IType<TValue>);
begin
  { Create a wrapper over the real type class and switch it }
  FKeyWrapperType := TObjectWrapperType<TKey>.Create(AKeyType);
  FValueWrapperType := TObjectWrapperType<TValue>.Create(AValueType);

  { Install overridden type }
  inherited InstallTypes(FKeyWrapperType, FValueWrapperType);
end;

function TObjectDoubleSortedBidiMap<TKey, TValue>.GetOwnsKeys: Boolean;
begin
  Result := FKeyWrapperType.AllowCleanup;
end;

function TObjectDoubleSortedBidiMap<TKey, TValue>.GetOwnsValues: Boolean;
begin
  Result := FValueWrapperType.AllowCleanup;
end;

procedure TObjectDoubleSortedBidiMap<TKey, TValue>.SetOwnsKeys(const Value: Boolean);
begin
  FKeyWrapperType.AllowCleanup := Value;
end;

procedure TObjectDoubleSortedBidiMap<TKey, TValue>.SetOwnsValues(const Value: Boolean);
begin
  FValueWrapperType.AllowCleanup := Value;
end;

end.
