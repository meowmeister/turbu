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
unit DeHL.Collections.Dictionary;
interface
uses SysUtils,
     DeHL.Base,
     DeHL.Types,
     DeHL.Serialization,
     DeHL.Exceptions,
     DeHL.Arrays,
     DeHL.Math.Primes,
     DeHL.KeyValuePair,
     DeHL.Collections.Base;

type
  { Generic Dictionary }
  TDictionary<TKey, TValue> = class(TEnexAssociativeCollection<TKey, TValue>, IDictionary<TKey, TValue>)
  private
  type
    { Generic Dictionary Pairs Enumerator }
    TPairEnumerator = class(TEnumerator<TKeyValuePair<TKey,TValue>>)
    private
      FVer         : Cardinal;
      FDict        : TDictionary<TKey, TValue>;
      FCurrentIndex: Integer;
      FValue       : TKeyValuePair<TKey,TValue>;

    public
      { Constructor }
      constructor Create(const ADict: TDictionary<TKey, TValue>);

      { Destructor }
      destructor Destroy(); override;

      function GetCurrent(): TKeyValuePair<TKey,TValue>; override;
      function MoveNext(): Boolean; override;
    end;

    { Generic Dictionary Keys Enumerator }
    TKeyEnumerator = class(TEnumerator<TKey>)
    private
      FVer         : Cardinal;
      FDict        : TDictionary<TKey, TValue>;
      FCurrentIndex: Integer;
      FValue       : TKey;
    public
      { Constructor }
      constructor Create(const ADict: TDictionary<TKey, TValue>);

      { Destructor }
      destructor Destroy(); override;

      function GetCurrent(): TKey; override;
      function MoveNext(): Boolean; override;
    end;

    { Generic Dictionary Values Enumerator }
    TValueEnumerator = class(TEnumerator<TValue>)
    private
      FVer         : Cardinal;
      FDict        : TDictionary<TKey, TValue>;
      FCurrentIndex: Integer;
      FValue       : TValue;
    public
      { Constructor }
      constructor Create(const ADict: TDictionary<TKey, TValue>);

      { Destructor }
      destructor Destroy(); override;

      function GetCurrent(): TValue; override;
      function MoveNext(): Boolean; override;
    end;

    TEntry = record
      FHashCode: Integer;
      FNext    : Integer;
      FKey     : TKey;
      FValue   : TValue;
    end;

    TBucketArray = array of Integer;
    TEntryArray = TArray<TEntry>;

  var
    FBucketArray  : TBucketArray;
    FEntryArray   : TEntryArray;

    FCount        : Integer;
    FFreeCount    : Integer;
    FFreeList     : Integer;

    FVer          : Cardinal;

    { Internal }
    procedure InitializeInternals(const Capacity: Cardinal);
    procedure Insert(const AKey: TKey; const AValue: TValue; const ShouldAdd: Boolean = true);
    function FindEntry(const AKey: TKey): Integer;
    procedure Resize();
    function Hash(const AKey: TKey): Integer;

  protected
    { Serialization overrides }
    procedure StartSerializing(const AData: TSerializationData); override;
    procedure StartDeserializing(const AData: TDeserializationData); override;
    procedure DeserializePair(const AKey: TKey; const AValue: TValue); override;

    { Enex }
    function GetCount(): Cardinal; override;

    { Key getter and setter }
    function GetItem(const Key: TKey): TValue;
    procedure SetItem(const Key: TKey; const Value: TValue);

  public type
    { Generic Dictionary Keys Collection }
    TKeyCollection = class(TEnexCollection<TKey>)
    private
      FDict        : TDictionary<TKey, TValue>;

    protected
      { Hidden }
      function GetCount(): Cardinal; override;

    public
      { Constructor }
      constructor Create(const ADict: TDictionary<TKey, TValue>);

      { Destructor }
      destructor Destroy(); override;

      { Property }
      property Count: Cardinal read GetCount;

      { IEnumerable/ ICollection support }
      function GetEnumerator(): IEnumerator<TKey>; override;

      { Copy-To }
      procedure CopyTo(var AArray: array of TKey); overload; override;
      procedure CopyTo(var AArray: array of TKey; const StartIndex: Cardinal); overload; override;
    end;

    { Generic Dictionary Values Collection }
    TValueCollection = class(TEnexCollection<TValue>)
    private
      FDict        : TDictionary<TKey, TValue>;

    protected
      { Hidden }
      function GetCount: Cardinal; override;

    public
      { Constructor }
      constructor Create(const ADict: TDictionary<TKey, TValue>);

      { Destructor }
      destructor Destroy(); override;

      { Property }
      property Count: Cardinal read GetCount;

      { IEnumerable/ ICollection support }
      function GetEnumerator(): IEnumerator<TValue>; override;

      { Copy-To }
      procedure CopyTo(var AArray: array of TValue); overload; override;
      procedure CopyTo(var AArray: array of TValue; const StartIndex: Cardinal); overload; override;
    end;

    { Constructors }
    constructor Create(); overload;
    constructor Create(const InitialCapacity: Cardinal); overload;
    constructor Create(const AEnumerable: IEnumerable<TKeyValuePair<TKey, TValue>>); overload;
    constructor Create(const AArray: array of TKeyValuePair<TKey, TValue>); overload;
    constructor Create(const AArray: TDynamicArray<TKeyValuePair<TKey, TValue>>); overload;
    constructor Create(const AArray: TFixedArray<TKeyValuePair<TKey, TValue>>); overload;

    constructor Create(const AKeyType: IType<TKey>; const AValueType: IType<TValue>); overload;
    constructor Create(const AKeyType: IType<TKey>; const AValueType: IType<TValue>; const InitialCapacity: Cardinal); overload;
    constructor Create(const AKeyType: IType<TKey>; const AValueType: IType<TValue>;
          const AEnumerable: IEnumerable<TKeyValuePair<TKey, TValue>>); overload;
    constructor Create(const AKeyType: IType<TKey>; const AValueType: IType<TValue>;
          const AArray: array of TKeyValuePair<TKey,TValue>); overload;
    constructor Create(const AKeyType: IType<TKey>; const AValueType: IType<TValue>;
          const AArray: TDynamicArray<TKeyValuePair<TKey,TValue>>); overload;
    constructor Create(const AKeyType: IType<TKey>; const AValueType: IType<TValue>;
          const AArray: TFixedArray<TKeyValuePair<TKey,TValue>>); overload;

    private var
      FKeyCollection: IEnexCollection<TKey>;
      FValueCollection: IEnexCollection<TValue>;

    public

    { Destructor }
    destructor Destroy(); override;

    {  Modification }
    procedure Clear();

    procedure Add(const APair: TKeyValuePair<TKey,TValue>); overload;
    procedure Add(const AKey: TKey; const AValue: TValue); overload;
    procedure Remove(const AKey: TKey); overload;

    { Lookup }
    function ContainsKey(const AKey: TKey): Boolean;
    function ContainsValue(const AValue: TValue): Boolean;
    function TryGetValue(const AKey: TKey; out FoundValue: TValue): Boolean;

    { Properties }
    property Items[const Key: TKey]: TValue read GetItem write SetItem; default;
    property Count: Cardinal read GetCount;
    property Keys: IEnexCollection<TKey> read FKeyCollection;
    property Values: IEnexCollection<TValue> read FValueCollection;

    { IEnumerable/ ICollection support }
    function GetEnumerator(): IEnumerator<TKeyValuePair<TKey,TValue>>; override;

    { Copy-To }
    procedure CopyTo(var AArray: array of TKeyValuePair<TKey,TValue>); overload; override;
    procedure CopyTo(var AArray: array of TKeyValuePair<TKey,TValue>; const StartIndex: Cardinal); overload; override;

    { Enex - associativity }
    function ValueForKey(const AKey: TKey): TValue; override;
    function KeyHasValue(const AKey: TKey; const AValue: TValue): Boolean; override;

    { Enex - selectors }
    function SelectKeys(): IEnexCollection<TKey>; override;
    function SelectValues(): IEnexCollection<TValue>; override;
  end;

  { The object variant }
  TObjectDictionary<TKey, TValue: class> = class sealed(TDictionary<TKey, TValue>)
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

{$IFNDEF BUG_URW1100}
type
  TBugReproducer = TDictionary<TTypeClass, String>;
{$ENDIF}

implementation

const
  DefaultArrayLength = 32;

{ TDictionary<TKey, TValue> }

procedure TDictionary<TKey, TValue>.Add(const APair: TKeyValuePair<TKey, TValue>);
begin
 { Call insert }
 Insert(APair.Key, APair.Value);
end;

procedure TDictionary<TKey, TValue>.Add(const AKey: TKey; const AValue: TValue);
begin
 { Call insert }
 Insert(AKey, AValue);
end;

procedure TDictionary<TKey, TValue>.Clear;
var
  I, K  : Cardinal;
  KC, VC, MKC, MVC: Boolean;
begin
  if FCount > 0 then
  begin
    for I := 0 to Length(FBucketArray) - 1 do
        FBucketArray[I] := -1;
  end;

  if Length(FEntryArray) > 0 then
  begin
    KC := (KeyType.Management() = tmManual);
    MKC:= (KeyType.Management() = tmCompiler);
    VC := (ValueType.Management() = tmManual);
    MVC := (ValueType.Management() = tmCompiler);

    if (KC or MKC or VC or MVC) then
    begin
      for I := 0 to Length(FEntryArray) - 1 do
      begin
        if FEntryArray[I].FHashCode >= 0 then
        begin
          { Either manually cleanup or tell compiler/RTL to do so! }
          if KC then
            KeyType.Cleanup(FEntryArray[I].FKey)
          else if MKC then
            FEntryArray[I].FKey := default(TKey);

          { Either manually cleanup or tell compiler/RTL to do so! }
          if VC then
            ValueType.Cleanup(FEntryArray[I].FValue)
          else if MVC then
            FEntryArray[I].FValue := default(TValue);
        end;
      end;
    end;

    FillChar(FEntryArray[0], Length(FEntryArray) * SizeOf(TEntry), 0);
  end;

  FFreeList := -1;
  FCount := 0;
  FFreeCount := 0;

  Inc(FVer);
end;

function TDictionary<TKey, TValue>.ContainsKey(const AKey: TKey): Boolean;
begin
  Result := (FindEntry(AKey) >= 0);
end;

function TDictionary<TKey, TValue>.ContainsValue(const AValue: TValue): Boolean;
var
  I: Integer;
begin
  Result := False;

  for I := 0 to FCount - 1 do
  begin
    if (FEntryArray[I].FHashCode >= 0) and (ValueType.AreEqual(FEntryArray[I].FValue, AValue)) then
       begin Result := True; Exit; end;

  end;
end;

procedure TDictionary<TKey, TValue>.CopyTo(var AArray: array of TKeyValuePair<TKey, TValue>);
begin
  { Call the more generic function }
  CopyTo(AArray, 0);
end;

procedure TDictionary<TKey, TValue>.CopyTo(
  var AArray: array of TKeyValuePair<TKey, TValue>; const StartIndex: Cardinal);
var
  I, X: Integer;
begin
  { Check for indexes }
  if StartIndex >= Cardinal(Length(AArray)) then
    ExceptionHelper.Throw_ArgumentOutOfRangeError('StartIndex');

  if (Cardinal(Length(AArray)) - StartIndex) < Count then
     ExceptionHelper.Throw_ArgumentOutOfSpaceError('AArray');

  X := StartIndex;

  for I := 0 to FCount - 1 do
  begin
    if (FEntryArray[I].FHashCode >= 0) then
    begin
       AArray[X] := TKeyValuePair<TKey, TValue>.Create(FEntryArray[I].FKey, FEntryArray[I].FValue);
       Inc(X);
    end;
  end;
end;

constructor TDictionary<TKey, TValue>.Create;
begin
  Create(TType<TKey>.Default, TType<TValue>.Default);
end;

constructor TDictionary<TKey, TValue>.Create(const InitialCapacity: Cardinal);
begin
  Create(TType<TKey>.Default, TType<TValue>.Default, InitialCapacity);
end;

constructor TDictionary<TKey, TValue>.Create(
  const AEnumerable: IEnumerable<TKeyValuePair<TKey, TValue>>);
begin
  Create(TType<TKey>.Default, TType<TValue>.Default, AEnumerable);
end;

constructor TDictionary<TKey, TValue>.Create(
  const AKeyType: IType<TKey>;
  const AValueType: IType<TValue>; const InitialCapacity: Cardinal);
begin
  inherited Create();

  { Initialize instance }
  if (AKeyType = nil) then
     ExceptionHelper.Throw_ArgumentNilError('AKeyType');

  if (AValueType = nil) then
     ExceptionHelper.Throw_ArgumentNilError('AValueType');

  { Install types }
  InstallTypes(AKeyType, AValueType);

  FKeyCollection := TKeyCollection.Create(Self);
  FValueCollection := TValueCollection.Create(Self);

  FVer := 0;
  FCount := 0;
  FFreeCount := 0;
  FFreeList := 0;

  InitializeInternals(DefaultArrayLength);
end;

constructor TDictionary<TKey, TValue>.Create(const AKeyType: IType<TKey>;
  const AValueType: IType<TValue>;
  const AEnumerable: IEnumerable<TKeyValuePair<TKey, TValue>>);
var
  V: TKeyValuePair<TKey, TValue>;
begin
  { Call upper constructor }
  Create(AKeyType, AValueType, DefaultArrayLength);

  if (AEnumerable = nil) then
     ExceptionHelper.Throw_ArgumentNilError('AEnumerable');

  { Pump in all items }
  for V in AEnumerable do
  begin
{$IFNDEF BUG_GENERIC_INCOMPAT_TYPES}
    Add(V);
{$ELSE}
    Add(V.Key, V.Value);
{$ENDIF}
  end;
end;

constructor TDictionary<TKey, TValue>.Create(
  const AKeyType: IType<TKey>;
  const AValueType: IType<TValue>);
begin
  { Call upper constructor }
  Create(AKeyType, AValueType, DefaultArrayLength);
end;

procedure TDictionary<TKey, TValue>.DeserializePair(const AKey: TKey; const AValue: TValue);
begin
  { Simple as hell ... }
  Add(AKey, AValue);
end;

destructor TDictionary<TKey, TValue>.Destroy;
begin
  { Clear first }
  Clear();

  inherited;
end;

function TDictionary<TKey, TValue>.FindEntry(const AKey: TKey): Integer;
var
  HashCode: Integer;
  I       : Integer;
begin
  Result := -1;

  if Length(FBucketArray) > 0 then
  begin
    { Generate the hash code }
    HashCode := Hash(AKey);

    I := FBucketArray[HashCode mod Length(FBucketArray)];

    while I >= 0 do
    begin
      if (FEntryArray[I].FHashCode = HashCode) and KeyType.AreEqual(FEntryArray[I].FKey, AKey) then
         begin Result := I; Exit; end;

      I := FEntryArray[I].FNext;
    end;
  end;
end;

function TDictionary<TKey, TValue>.GetCount: Cardinal;
begin
  Result := (FCount - FFreeCount);
end;

function TDictionary<TKey, TValue>.GetEnumerator: IEnumerator<TKeyValuePair<TKey, TValue>>;
begin
  Result := TDictionary<TKey, TValue>.TPairEnumerator.Create(Self);
end;

function TDictionary<TKey, TValue>.GetItem(const Key: TKey): TValue;
begin
  if not TryGetValue(Key, Result) then
    ExceptionHelper.Throw_KeyNotFoundError(KeyType.GetString(Key));
end;

function TDictionary<TKey, TValue>.Hash(const AKey: TKey): Integer;
const
  PositiveMask = not Integer($80000000);
begin
  Result := PositiveMask and ((PositiveMask and KeyType.GenerateHashCode(AKey)) + 1);
end;

procedure TDictionary<TKey, TValue>.InitializeInternals(
  const Capacity: Cardinal);
var
  XPrime: Integer;
  I    : Integer;
begin
  XPrime := Prime.GetNearestProgressionPositive(Capacity);

  SetLength(FBucketArray, XPrime);
  SetLength(FEntryArray, XPrime);

  for I := 0 to XPrime - 1 do
  begin
    FBucketArray[I] := -1;
    FEntryArray[I].FHashCode := -1;
  end;

  FFreeList := -1;
end;

procedure TDictionary<TKey, TValue>.Insert(const AKey: TKey;
  const AValue: TValue; const ShouldAdd: Boolean);
var
  FreeList: Integer;
  Index   : Integer;
  HashCode: Integer;
  I       : Integer;
begin
  FreeList := 0;

  if Length(FBucketArray) = 0 then
     InitializeInternals(0);

  { Generate the hash code }
  HashCode := Hash(AKey);
  Index := HashCode mod Length(FBucketArray);

  I := FBucketArray[Index];

  while I >= 0 do
  begin
    if (FEntryArray[I].FHashCode = HashCode) and KeyType.AreEqual(FEntryArray[I].FKey, AKey) then
    begin
      if (ShouldAdd) then
        ExceptionHelper.Throw_DuplicateKeyError('AKey');

      FEntryArray[I].FValue := AValue;
      Inc(FVer);
      Exit;
    end;

    { Move to next }
    I := FEntryArray[I].FNext;
  end;

  { Adjust free spaces }
  if FFreeCount > 0 then
  begin
    FreeList := FFreeList;
    FFreeList := FEntryArray[FreeList].FNext;

    Dec(FFreeCount);
  end else
  begin
    { Adjust index if there is not enough free space }
    if FCount = Length(FEntryArray) then
    begin
      Resize();
      Index := HashCode mod Length(FBucketArray);
    end;

    FreeList := FCount;
    Inc(FCount);
  end;

  { Insert the element at the right position and adjust arrays }
  FEntryArray[FreeList].FHashCode := HashCode;
  FEntryArray[FreeList].FKey := AKey;
  FEntryArray[FreeList].FValue := AValue;
  FEntryArray[FreeList].FNext := FBucketArray[Index];

  FBucketArray[Index] := FreeList;
  Inc(FVer);
end;

function TDictionary<TKey, TValue>.KeyHasValue(const AKey: TKey; const AValue: TValue): Boolean;
var
  LValue: TValue;
begin
  Result := TryGetValue(AKey, LValue) and ValueType.AreEqual(LValue, AValue);
end;

procedure TDictionary<TKey, TValue>.Remove(const AKey: TKey);
var
  HashCode: Integer;
  Index   : Integer;
  I       : Integer;
  RemIndex: Integer;
begin
  if Length(FBucketArray) > 0 then
  begin
    { Generate the hash code }
    HashCode := Hash(AKey);

    Index := HashCode mod Length(FBucketArray);
    RemIndex := -1;

    I := FBucketArray[Index];

    while I >= 0 do
    begin
      if (FEntryArray[I].FHashCode = HashCode) and KeyType.AreEqual(FEntryArray[I].FKey, AKey) then
      begin

        if RemIndex < 0 then
        begin
          FBucketArray[Index] := FEntryArray[I].FNext;
        end else
        begin
          FEntryArray[RemIndex].FNext := FEntryArray[I].FNext;
        end;

        { Cleanup required? }
        if ValueType.Management() = tmManual then
           ValueType.Cleanup(FEntryArray[I].FValue);

        FEntryArray[I].FHashCode := -1;
        FEntryArray[I].FNext := FFreeList;
        FEntryArray[I].FKey := default(TKey);
        FEntryArray[I].FValue := default(TValue);

        FFreeList := I;
        Inc(FFreeCount);
        Inc(FVer);

        Exit;
      end;

      RemIndex := I;
      I := FEntryArray[I].FNext;
    end;

  end;
end;

procedure TDictionary<TKey, TValue>.Resize;
var
  XPrime: Integer;
  I     : Integer;
  Index : Integer;
  NArr  : TBucketArray;
begin
  XPrime := Prime.GetNearestProgressionPositive(FCount * 2);

  SetLength(NArr, XPrime);
  for I := 0 to Length(NArr) - 1 do
    NArr[I] := -1;

  SetLength(FEntryArray, XPrime);

  for I := 0 to FCount - 1 do
  begin
    Index := FEntryArray[I].FHashCode mod XPrime;
    FEntryArray[I].FNext := NArr[Index];
    NArr[Index] := I;
  end;

  { Reset bucket array }
  FBucketArray := NArr;
end;

function TDictionary<TKey, TValue>.SelectKeys: IEnexCollection<TKey>;
begin
  Result := Keys;
end;

function TDictionary<TKey, TValue>.SelectValues: IEnexCollection<TValue>;
begin
  Result := Values;
end;

procedure TDictionary<TKey, TValue>.SetItem(const Key: TKey;
  const Value: TValue);
begin
  { Simply call insert }
  Insert(Key, Value, false);
end;

procedure TDictionary<TKey, TValue>.StartDeserializing(const AData: TDeserializationData);
begin
  // Do nothing, just say that I am here and I can be serialized
end;

procedure TDictionary<TKey, TValue>.StartSerializing(const AData: TSerializationData);
begin
  // Do nothing, just say that I am here and I can be serialized
end;

function TDictionary<TKey, TValue>.TryGetValue(const AKey: TKey; out FoundValue: TValue): Boolean;
var
  Index: Integer;
begin
  Index := FindEntry(AKey);

  if Index >= 0 then
     begin
       FoundValue := FEntryArray[Index].FValue;
       Exit(True);
     end;

  { Key not found, simply fail }
  FoundValue := Default(TValue);
  Result := False;
end;

function TDictionary<TKey, TValue>.ValueForKey(const AKey: TKey): TValue;
begin
  Result := GetItem(AKey);
end;

constructor TDictionary<TKey, TValue>.Create(
  const AArray: array of TKeyValuePair<TKey, TValue>);
begin
  Create(TType<TKey>.Default, TType<TValue>.Default, AArray);
end;

constructor TDictionary<TKey, TValue>.Create(
  const AKeyType: IType<TKey>;
  const AValueType: IType<TValue>;
  const AArray: array of TKeyValuePair<TKey, TValue>);
var
  I: Integer;
begin
  { Call upper constructor }
  Create(AKeyType, AValueType, DefaultArrayLength);

  { Copy all items in }
  for I := 0 to Length(AArray) - 1 do
  begin
    Add(AArray[I]);
  end;
end;

constructor TDictionary<TKey, TValue>.Create(
  const AArray: TDynamicArray<TKeyValuePair<TKey, TValue>>);
begin
  Create(TType<TKey>.Default, TType<TValue>.Default, AArray);
end;

constructor TDictionary<TKey, TValue>.Create(
  const AArray: TFixedArray<TKeyValuePair<TKey, TValue>>);
begin
  Create(TType<TKey>.Default, TType<TValue>.Default, AArray);
end;

constructor TDictionary<TKey, TValue>.Create(const AKeyType: IType<TKey>;
  const AValueType: IType<TValue>;
  const AArray: TDynamicArray<TKeyValuePair<TKey, TValue>>);
var
  I: Cardinal;
begin
  { Call upper constructor }
  Create(AKeyType, AValueType, DefaultArrayLength);

  { Copy all items in }
  if AArray.Length > 0 then
    for I := 0 to AArray.Length - 1 do
    begin
{$IFNDEF BUG_GENERIC_INCOMPAT_TYPES}
      Add(AArray[I]);
{$ELSE}
      Add(AArray[I].Key, AArray[I].Value);
{$ENDIF}
    end;
end;

constructor TDictionary<TKey, TValue>.Create(const AKeyType: IType<TKey>;
  const AValueType: IType<TValue>;
  const AArray: TFixedArray<TKeyValuePair<TKey, TValue>>);
var
  I: Cardinal;
begin
  { Call upper constructor }
  Create(AKeyType, AValueType, DefaultArrayLength);

  { Copy all items in }
  if AArray.Length > 0 then
    for I := 0 to AArray.Length - 1 do
    begin
{$IFNDEF BUG_GENERIC_INCOMPAT_TYPES}
      Add(AArray[I]);
{$ELSE}
      Add(AArray[I].Key, AArray[I].Value);
{$ENDIF}
    end;
end;

{ TDictionary<TKey, TValue>.TPairEnumerator }

constructor TDictionary<TKey, TValue>.TPairEnumerator.Create(const ADict: TDictionary<TKey, TValue>);
begin
  { Initialize }
  FDict := ADict;
  KeepObjectAlive(FDict);
  
  FCurrentIndex := 0;
  FVer := ADict.FVer;
end;

destructor TDictionary<TKey, TValue>.TPairEnumerator.Destroy;
begin
  ReleaseObject(FDict);
  inherited;
end;

function TDictionary<TKey, TValue>.TPairEnumerator.GetCurrent: TKeyValuePair<TKey,TValue>;
begin
  if FVer <> FDict.FVer then
     ExceptionHelper.Throw_CollectionChangedError();

  Result := FValue;
end;

function TDictionary<TKey, TValue>.TPairEnumerator.MoveNext: Boolean;
begin
  if FVer <> FDict.FVer then
     ExceptionHelper.Throw_CollectionChangedError();

  while FCurrentIndex < FDict.FCount do
  begin
    if FDict.FEntryArray[FCurrentIndex].FHashCode >= 0 then
    begin
      FValue := TKeyValuePair<TKey, TValue>.Create(FDict.FEntryArray[FCurrentIndex].FKey,
                  FDict.FEntryArray[FCurrentIndex].FValue);

      Inc(FCurrentIndex);
      Result := True;
      Exit;
    end;

    Inc(FCurrentIndex);
  end;

  FCurrentIndex := FDict.FCount + 1;
  Result := False;
end;

{ TDictionary<TKey, TValue>.TKeyEnumerator }

constructor TDictionary<TKey, TValue>.TKeyEnumerator.Create(const ADict: TDictionary<TKey, TValue>);
begin
  { Initialize }
  FDict := ADict;
  KeepObjectAlive(FDict);
  
  FCurrentIndex := 0;
  FVer := ADict.FVer;
  FValue := default(TKey);
end;

destructor TDictionary<TKey, TValue>.TKeyEnumerator.Destroy;
begin
  ReleaseObject(FDict);
  inherited;
end;

function TDictionary<TKey, TValue>.TKeyEnumerator.GetCurrent: TKey;
begin
  if FVer <> FDict.FVer then
     ExceptionHelper.Throw_CollectionChangedError();

  Result := FValue;
end;

function TDictionary<TKey, TValue>.TKeyEnumerator.MoveNext: Boolean;
begin
  if FVer <> FDict.FVer then
     ExceptionHelper.Throw_CollectionChangedError();

  while FCurrentIndex < FDict.FCount do
  begin
    if FDict.FEntryArray[FCurrentIndex].FHashCode >= 0 then
    begin
      FValue := FDict.FEntryArray[FCurrentIndex].FKey;

      Inc(FCurrentIndex);
      Result := True;
      Exit;
    end;

    Inc(FCurrentIndex);
  end;

  FCurrentIndex := FDict.FCount + 1;
  Result := False;
end;


{ TDictionary<TKey, TValue>.TValueEnumerator }

constructor TDictionary<TKey, TValue>.TValueEnumerator.Create(const ADict: TDictionary<TKey, TValue>);
begin
  { Initialize }
  FDict := ADict;
  KeepObjectAlive(FDict);

  FCurrentIndex := 0;
  FVer := ADict.FVer;
end;

destructor TDictionary<TKey, TValue>.TValueEnumerator.Destroy;
begin
  ReleaseObject(FDict);
  inherited;
end;

function TDictionary<TKey, TValue>.TValueEnumerator.GetCurrent: TValue;
begin
  if FVer <> FDict.FVer then
     ExceptionHelper.Throw_CollectionChangedError();

  Result := FValue;
end;

function TDictionary<TKey, TValue>.TValueEnumerator.MoveNext: Boolean;
begin
  if FVer <> FDict.FVer then
     ExceptionHelper.Throw_CollectionChangedError();

  while FCurrentIndex < FDict.FCount do
  begin
    if FDict.FEntryArray[FCurrentIndex].FHashCode >= 0 then
    begin
      FValue := FDict.FEntryArray[FCurrentIndex].FValue;

      Inc(FCurrentIndex);
      Result := True;
      Exit;
    end;

    Inc(FCurrentIndex);
  end;

  FCurrentIndex := FDict.FCount + 1;
  Result := False;
end;

{ TDictionary<TKey, TValue>.TKeyCollection }

constructor TDictionary<TKey, TValue>.TKeyCollection.Create(const ADict: TDictionary<TKey, TValue>);
begin
  { Initialize }
  FDict := ADict;

  { Install key type }
  InstallType(FDict.KeyType);
end;

destructor TDictionary<TKey, TValue>.TKeyCollection.Destroy;
begin
  inherited;
end;

function TDictionary<TKey, TValue>.TKeyCollection.GetCount: Cardinal;
begin
  { Number of elements is the same as key }
  Result := FDict.Count;
end;

function TDictionary<TKey, TValue>.TKeyCollection.GetEnumerator: IEnumerator<TKey>;
begin
  Result := TKeyEnumerator.Create(Self.FDict);
end;

procedure TDictionary<TKey, TValue>.TKeyCollection.CopyTo(var AArray: array of TKey);
begin
  { Call more generic function }
  CopyTo(AArray, 0);
end;

procedure TDictionary<TKey, TValue>.TKeyCollection.CopyTo(var AArray: array of TKey; const StartIndex: Cardinal);
var
  I, X: Integer;
begin
  { Check for indexes }
  if StartIndex >= Cardinal(Length(AArray)) then
    ExceptionHelper.Throw_ArgumentOutOfRangeError('StartIndex');

  if (Cardinal(Length(AArray)) - StartIndex) < FDict.Count then
     ExceptionHelper.Throw_ArgumentOutOfSpaceError('AArray');

  X := StartIndex;

  for I := 0 to FDict.FCount - 1 do
  begin
    if (FDict.FEntryArray[I].FHashCode >= 0) then
    begin
       AArray[X] := FDict.FEntryArray[I].FKey;
       Inc(X);
    end;
  end;
end;

{ TDictionary<TKey, TValue>.TValueCollection }

constructor TDictionary<TKey, TValue>.TValueCollection.Create(const ADict: TDictionary<TKey, TValue>);
begin
  { Initialize }
  FDict := ADict;

  { Install key type }
  InstallType(FDict.ValueType);
end;

destructor TDictionary<TKey, TValue>.TValueCollection.Destroy;
begin
  inherited;
end;

function TDictionary<TKey, TValue>.TValueCollection.GetCount: Cardinal;
begin
  { Number of elements is the same as key }
  Result := FDict.Count;
end;

function TDictionary<TKey, TValue>.TValueCollection.GetEnumerator: IEnumerator<TValue>;
begin
  Result := TValueEnumerator.Create(Self.FDict);
end;

procedure TDictionary<TKey, TValue>.TValueCollection.CopyTo(var AArray: array of TValue);
begin
  { Call more generic function }
  CopyTo(AArray, 0);
end;

procedure TDictionary<TKey, TValue>.TValueCollection.CopyTo(var AArray: array of TValue; const StartIndex: Cardinal);
var
  I, X: Integer;
begin
  if StartIndex >= Cardinal(Length(AArray)) then
    ExceptionHelper.Throw_ArgumentOutOfRangeError('StartIndex');

  { Check for indexes }
  if (Cardinal(Length(AArray)) - StartIndex) < FDict.Count then
     ExceptionHelper.Throw_ArgumentOutOfSpaceError('AArray');

  X := StartIndex;

  for I := 0 to FDict.FCount - 1 do
  begin
    if (FDict.FEntryArray[I].FHashCode >= 0) then
    begin
       AArray[X] := FDict.FEntryArray[I].FValue;
       Inc(X);
    end;
  end;
end;


{ TObjectDictionary<TKey, TValue> }

function TObjectDictionary<TKey, TValue>.GetOwnsKeys: Boolean;
begin
  Result := FKeyWrapperType.AllowCleanup;
end;

function TObjectDictionary<TKey, TValue>.GetOwnsValues: Boolean;
begin
  Result := FValueWrapperType.AllowCleanup;
end;

procedure TObjectDictionary<TKey, TValue>.InstallTypes(const AKeyType: IType<TKey>; const AValueType: IType<TValue>);
begin
  { Create a wrapper over the real type class and switch it }
  FKeyWrapperType := TObjectWrapperType<TKey>.Create(AKeyType);
  FValueWrapperType := TObjectWrapperType<TValue>.Create(AValueType);

  { Install overridden type }
  inherited InstallTypes(FKeyWrapperType, FValueWrapperType);
end;

procedure TObjectDictionary<TKey, TValue>.SetOwnsKeys(const Value: Boolean);
begin
  FKeyWrapperType.AllowCleanup := Value;
end;

procedure TObjectDictionary<TKey, TValue>.SetOwnsValues(const Value: Boolean);
begin
  FValueWrapperType.AllowCleanup := Value;
end;

end.
