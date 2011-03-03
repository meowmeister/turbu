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
unit DeHL.Collections.DoubleSortedDistinctMultiMap;
interface
uses SysUtils,
     DeHL.Base,
     DeHL.Types,
     DeHL.StrConsts,
     DeHL.Exceptions,
     DeHL.KeyValuePair,
     DeHL.Serialization,
     DeHL.Arrays,
     DeHL.Collections.Base,
     DeHL.Collections.SortedDistinctMultiMap,
     DeHL.Collections.SortedSet;

type
  { Multi-Map based on a sorted dictionary and a list }
  TDoubleSortedDistinctMultiMap<TKey, TValue> = class(TSortedDistinctMultiMap<TKey, TValue>)
  private
    FAscValues: Boolean;

  protected
    { Provide our implementations }
    function CreateSet(const AValueType: IType<TValue>): ISet<TValue>; override;

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
  end;

  { The object variant }
  TObjectDoubleSortedDistinctMultiMap<TKey, TValue: class> = class sealed(TDoubleSortedDistinctMultiMap<TKey, TValue>)
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

{ TDoubleSortedDistinctMultiMap<TKey, TValue> }

constructor TDoubleSortedDistinctMultiMap<TKey, TValue>.Create(
  const AArray: TDynamicArray<TKeyValuePair<TKey, TValue>>; const AscendingKeys,
  AscendingValues: Boolean);
begin
  { Do da dew and continue! }
  FAscValues := AscendingValues;
  inherited Create(AArray, AscendingKeys);
end;

constructor TDoubleSortedDistinctMultiMap<TKey, TValue>.Create(
  const AArray: TFixedArray<TKeyValuePair<TKey, TValue>>; const AscendingKeys,
  AscendingValues: Boolean);
begin
  { Do da dew and continue! }
  FAscValues := AscendingValues;
  inherited Create(AArray, AscendingKeys);
end;

constructor TDoubleSortedDistinctMultiMap<TKey, TValue>.Create(
  const AArray: array of TKeyValuePair<TKey, TValue>; const AscendingKeys,
  AscendingValues: Boolean);
begin
  { Do da dew and continue! }
  FAscValues := AscendingValues;
  inherited Create(AArray, AscendingKeys);
end;

constructor TDoubleSortedDistinctMultiMap<TKey, TValue>.Create(const AscendingKeys, AscendingValues: Boolean);
begin
  { Do da dew and continue! }
  FAscValues := AscendingValues;
  inherited Create(AscendingKeys);
end;

constructor TDoubleSortedDistinctMultiMap<TKey, TValue>.Create(
  const AEnumerable: IEnumerable<TKeyValuePair<TKey, TValue>>;
  const AscendingKeys, AscendingValues: Boolean);
begin
  { Do da dew and continue! }
  FAscValues := AscendingValues;
  inherited Create(AEnumerable, AscendingKeys);
end;

constructor TDoubleSortedDistinctMultiMap<TKey, TValue>.Create(
  const AKeyType: IType<TKey>; const AValueType: IType<TValue>;
  const AArray: TDynamicArray<TKeyValuePair<TKey, TValue>>; const AscendingKeys,
  AscendingValues: Boolean);
begin
  { Do da dew and continue! }
  FAscValues := AscendingValues;
  inherited Create(AKeyType, AValueType, AArray, AscendingKeys);
end;

constructor TDoubleSortedDistinctMultiMap<TKey, TValue>.Create(
  const AKeyType: IType<TKey>; const AValueType: IType<TValue>;
  const AArray: TFixedArray<TKeyValuePair<TKey, TValue>>; const AscendingKeys,
  AscendingValues: Boolean);
begin
  { Do da dew and continue! }
  FAscValues := AscendingValues;
  inherited Create(AKeyType, AValueType, AArray, AscendingKeys);
end;

constructor TDoubleSortedDistinctMultiMap<TKey, TValue>.Create(
  const AKeyType: IType<TKey>; const AValueType: IType<TValue>;
  const AArray: array of TKeyValuePair<TKey, TValue>; const AscendingKeys,
  AscendingValues: Boolean);
begin
  { Do da dew and continue! }
  FAscValues := AscendingValues;
  inherited Create(AKeyType, AValueType, AArray, AscendingKeys);
end;

constructor TDoubleSortedDistinctMultiMap<TKey, TValue>.Create(
  const AKeyType: IType<TKey>; const AValueType: IType<TValue>;
  const AscendingKeys, AscendingValues: Boolean);
begin
  { Do da dew and continue! }
  FAscValues := AscendingValues;
  inherited Create(AKeyType, AValueType, AscendingKeys);
end;

constructor TDoubleSortedDistinctMultiMap<TKey, TValue>.Create(
  const AKeyType: IType<TKey>; const AValueType: IType<TValue>;
  const AEnumerable: IEnumerable<TKeyValuePair<TKey, TValue>>;
  const AscendingKeys, AscendingValues: Boolean);
begin
  { Do da dew and continue! }
  FAscValues := AscendingValues;
  inherited Create(AKeyType, AValueType, AEnumerable, AscendingKeys);
end;

function TDoubleSortedDistinctMultiMap<TKey, TValue>.CreateSet(const AValueType: IType<TValue>): ISet<TValue>;
begin
  { Create a simple list }
  Result := TSortedSet<TValue>.Create(AValueType, FAscValues);
end;

procedure TDoubleSortedDistinctMultiMap<TKey, TValue>.DeserializePair(const AKey: TKey; const AValue: TValue);
begin
  { Write the ascending sign }
  Add(AKey, AValue);
end;

procedure TDoubleSortedDistinctMultiMap<TKey, TValue>.StartDeserializing(const AData: TDeserializationData);
var
  LAscKeys, LAscValues: Boolean;
begin
  { Try to obtain the ascending sign }
  AData.GetValue(SSerAscendingKeys, LAscKeys);
  AData.GetValue(SSerAscendingValues, LAscValues);

  { Call the constructor in this instance to initialize myself first }
  Create(LAscKeys, LAscValues);
end;

procedure TDoubleSortedDistinctMultiMap<TKey, TValue>.StartSerializing(const AData: TSerializationData);
begin
  inherited;

  { Write the ascending sign }
  AData.AddValue(SSerAscendingValues, FAscValues);
end;

{ TObjectDoubleSortedDistinctMultiMap<TKey, TValue> }

procedure TObjectDoubleSortedDistinctMultiMap<TKey, TValue>.InstallTypes(const AKeyType: IType<TKey>; const AValueType: IType<TValue>);
begin
  { Create a wrapper over the real type class and switch it }
  FKeyWrapperType := TObjectWrapperType<TKey>.Create(AKeyType);
  FValueWrapperType := TObjectWrapperType<TValue>.Create(AValueType);

  { Install overridden type }
  inherited InstallTypes(FKeyWrapperType, FValueWrapperType);
end;

function TObjectDoubleSortedDistinctMultiMap<TKey, TValue>.GetOwnsKeys: Boolean;
begin
  Result := FKeyWrapperType.AllowCleanup;
end;

function TObjectDoubleSortedDistinctMultiMap<TKey, TValue>.GetOwnsValues: Boolean;
begin
  Result := FValueWrapperType.AllowCleanup;
end;

procedure TObjectDoubleSortedDistinctMultiMap<TKey, TValue>.SetOwnsKeys(const Value: Boolean);
begin
  FKeyWrapperType.AllowCleanup := Value;
end;

procedure TObjectDoubleSortedDistinctMultiMap<TKey, TValue>.SetOwnsValues(const Value: Boolean);
begin
  FValueWrapperType.AllowCleanup := Value;
end;

end.
