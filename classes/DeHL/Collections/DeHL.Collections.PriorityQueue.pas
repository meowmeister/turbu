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
unit DeHL.Collections.PriorityQueue;
interface
uses SysUtils,
     DeHL.Base,
     DeHL.Types,
     DeHL.Serialization,
     DeHL.Exceptions,
     DeHL.Arrays,
     DeHL.StrConsts,
     DeHL.KeyValuePair,
     DeHL.Collections.Base;

type
  { Priority Queue }
  TPriorityQueue<TPriority, TValue> = class(TEnexAssociativeCollection<TPriority, TValue>,
    IPriorityQueue<TPriority, TValue>, IDynamic)
  private type
    { Internal storage }
    TPriorityPair = record
      FPriority: TPriority;
      FValue: TValue;
    end;

    { Generic List Enumerator }
    TPairEnumerator = class(TEnumerator<TKeyValuePair<TPriority, TValue>>)
    private
      FVer: Cardinal;
      FQueue: TPriorityQueue<TPriority, TValue>;
      FCurrentIndex: Cardinal;

    public
      { Constructor }
      constructor Create(const AQueue: TPriorityQueue<TPriority, TValue>);

      { Destructor }
      destructor Destroy(); override;

      function GetCurrent(): TKeyValuePair<TPriority, TValue>; override;
      function MoveNext(): Boolean; override;
    end;

  private
    FCount: Cardinal;
    FVer: Cardinal;
    FSign: Integer;
    FArray: TArray<TPriorityPair>;

    { Used internally to remove items from queue }
    function RemoveAt(const AIndex: Cardinal): TPriorityPair;

  protected
    { Serialization overrides }
    procedure StartSerializing(const AData: TSerializationData); override;
    procedure StartDeserializing(const AData: TDeserializationData); override;
    procedure DeserializePair(const AKey: TPriority; const AValue: TValue); override;

    { ICollection support/hidden }
    function GetCount(): Cardinal; override;

    { Gets the current capacity of the collection }
    function GetCapacity(): Cardinal;
  public
    { Constructors }
    constructor Create(const Ascending: Boolean = true); overload;
    constructor Create(const InitialCapacity: Cardinal; const Ascending: Boolean = true); overload;
    constructor Create(const AEnumerable: IEnumerable<TKeyValuePair<TPriority, TValue>>; const Ascending: Boolean = true); overload;
    constructor Create(const AArray: array of TKeyValuePair<TPriority, TValue>; const Ascending: Boolean = true); overload;
    constructor Create(const AArray: TDynamicArray<TKeyValuePair<TPriority, TValue>>; const Ascending: Boolean = true); overload;
    constructor Create(const AArray: TFixedArray<TKeyValuePair<TPriority, TValue>>; const Ascending: Boolean = true); overload;

    constructor Create(const APriorityType: IType<TPriority>; const AValueType: IType<TValue>;
      const Ascending: Boolean = true); overload;
    constructor Create(const APriorityType: IType<TPriority>; const AValueType: IType<TValue>;
      const InitialCapacity: Cardinal; const Ascending: Boolean = true); overload;
    constructor Create(const APriorityType: IType<TPriority>; const AValueType: IType<TValue>;
      const AEnumerable: IEnumerable<TKeyValuePair<TPriority, TValue>>; const Ascending: Boolean = true); overload;
    constructor Create(const APriorityType: IType<TPriority>; const AValueType: IType<TValue>;
      const AArray: array of TKeyValuePair<TPriority, TValue>; const Ascending: Boolean = true); overload;
    constructor Create(const APriorityType: IType<TPriority>; const AValueType: IType<TValue>;
      const AArray: TDynamicArray<TKeyValuePair<TPriority, TValue>>; const Ascending: Boolean = true); overload;
    constructor Create(const APriorityType: IType<TPriority>; const AValueType: IType<TValue>;
      const AArray: TFixedArray<TKeyValuePair<TPriority, TValue>>; const Ascending: Boolean = true); overload;

    { Destructor }
    destructor Destroy(); override;

    { IPriorityQueue }
    procedure Clear();
    function Contains(const AValue: TValue): Boolean;

    procedure Enqueue(const AValue: TValue); overload;
    procedure Enqueue(const AValue: TValue; const APriority: TPriority); overload;
    function Dequeue(): TValue; overload;
    function Peek(): TValue; overload;

    { Properties }
    property Count: Cardinal read FCount;
    property Capacity: Cardinal read GetCapacity;

    { IEnumerable/ ICollection support }
    function GetEnumerator() : IEnumerator<TKeyValuePair<TPriority, TValue>>; override;

    { Grow/Shrink }
    procedure Shrink();
    procedure Grow();

    { Enex: Copy-To }
    procedure CopyTo(var AArray: array of TKeyValuePair<TPriority, TValue>); overload; override;
    procedure CopyTo(var AArray: array of TKeyValuePair<TPriority, TValue>; const StartIndex: Cardinal); overload; override;

    { Enex - Associative collection }
    function MaxKey(): TPriority; override;
  end;

  { The object variant }
  TObjectPriorityQueue<TPriority, TValue: class> = class sealed(TPriorityQueue<TPriority, TValue>)
  private
    FPriorityWrapperType: TObjectWrapperType<TPriority>;
    FValueWrapperType: TObjectWrapperType<TValue>;

    { Getters/Setters for OwnsKeys }
    function GetOwnsPriorities: Boolean;
    procedure SetOwnsPriorities(const Value: Boolean);

    { Getters/Setters for OwnsValues }
    function GetOwnsValues: Boolean;
    procedure SetOwnsValues(const Value: Boolean);

  protected
    { Override in descendants to support proper stuff }
    procedure InstallTypes(const AKeyType: IType<TPriority>; const AValueType: IType<TValue>); override;

  public
    { Object owning }
    property OwnsPriorities: Boolean read GetOwnsPriorities write SetOwnsPriorities;
    property OwnsValues: Boolean read GetOwnsValues write SetOwnsValues;
  end;

implementation

const
  DefaultArrayLength = 8;

{ TPriorityQueue<TPriority, TValue> }

procedure TPriorityQueue<TPriority, TValue>.Clear;
var
  I: Cardinal;
  PC, VC: Boolean;
begin
  PC := (KeyType <> nil) and (KeyType.Management = tmManual);
  VC := (ValueType <> nil) and (ValueType.Management = tmManual);

  { Cleanup the array }
  if (Length(FArray) > 0) and (PC or VC) then
    for I := 0 to Length(FArray) - 1 do
    begin
      if PC then
        KeyType.Cleanup(FArray[I].FPriority);

      if VC then
        ValueType.Cleanup(FArray[I].FValue);
    end;

  { Dispose of all the stuff }
  Inc(FVer);
  FCount := 0;
end;

function TPriorityQueue<TPriority, TValue>.Contains(const AValue: TValue): Boolean;
var
  I: Cardinal;
begin
  { Check whether the thing contains what we need }
  if FCount > 0 then
    for I := 0 to FCount - 1 do
      if ValueType.AreEqual(FArray[I].FValue, AValue) then
        Exit(true);

  { Nope ... }
  Result := false;
end;

procedure TPriorityQueue<TPriority, TValue>.CopyTo(var AArray: array of TKeyValuePair<TPriority, TValue>; const StartIndex: Cardinal);
var
  I: Cardinal;
begin
  { Check for indexes }
  if StartIndex >= Cardinal(Length(AArray)) then
    ExceptionHelper.Throw_ArgumentOutOfRangeError('StartIndex');

  if (Cardinal(Length(AArray)) - StartIndex) < FCount then
     ExceptionHelper.Throw_ArgumentOutOfSpaceError('AArray');

  { Copy the stuff in }
  for I := 0 to FCount - 1 do
    AArray[StartIndex + I] := TKeyValuePair<TPriority, TValue>.Create(FArray[I].FPriority, FArray[I].FValue);
end;

procedure TPriorityQueue<TPriority, TValue>.CopyTo(var AArray: array of TKeyValuePair<TPriority, TValue>);
begin
  { Call upper method }
  CopyTo(AArray, 0);
end;

constructor TPriorityQueue<TPriority, TValue>.Create(const AArray: array of TKeyValuePair<TPriority, TValue>;
  const Ascending: Boolean);
begin
  { Call upper constructor }
  Create(TType<TPriority>.Default, TType<TValue>.Default, AArray, Ascending);
end;

constructor TPriorityQueue<TPriority, TValue>.Create(const AEnumerable: IEnumerable<TKeyValuePair<TPriority, TValue>>;
  const Ascending: Boolean);
begin
  { Call upper constructor }
  Create(TType<TPriority>.Default, TType<TValue>.Default, AEnumerable, Ascending);
end;

constructor TPriorityQueue<TPriority, TValue>.Create(const Ascending: Boolean);
begin
  { Call upper constructor }
  Create(TType<TPriority>.Default, TType<TValue>.Default, DefaultArrayLength, Ascending);
end;

constructor TPriorityQueue<TPriority, TValue>.Create(const AArray: TDynamicArray<TKeyValuePair<TPriority, TValue>>;
  const Ascending: Boolean);
begin
  { Call upper constructor }
  Create(TType<TPriority>.Default, TType<TValue>.Default, AArray, Ascending);
end;

constructor TPriorityQueue<TPriority, TValue>.Create(
  const APriorityType: IType<TPriority>;
  const AValueType: IType<TValue>;
  const AArray: array of TKeyValuePair<TPriority, TValue>;
  const Ascending: Boolean);
var
  I: Cardinal;
begin
  { Call upper constructor }
  Create(APriorityType, AValueType, DefaultArrayLength, Ascending);

  { Copy all items in }
  if Length(AArray) > 0 then
    for I := 0 to Length(AArray) - 1 do
      Enqueue(AArray[I].Value, AArray[I].Key);
end;

constructor TPriorityQueue<TPriority, TValue>.Create(
  const APriorityType: IType<TPriority>;
  const AValueType: IType<TValue>;
  const AArray: TDynamicArray<TKeyValuePair<TPriority, TValue>>;
  const Ascending: Boolean);
var
  I: Cardinal;
begin
  { Call upper constructor }
  Create(APriorityType, AValueType, DefaultArrayLength, Ascending);

  { Copy all items in }
  if AArray.Length > 0 then
    for I := 0 to AArray.Length - 1 do
      Enqueue(AArray[I].Value, AArray[I].Key);
end;

constructor TPriorityQueue<TPriority, TValue>.Create(
  const APriorityType: IType<TPriority>;
  const AValueType: IType<TValue>;
  const AArray: TFixedArray<TKeyValuePair<TPriority, TValue>>;
  const Ascending: Boolean);
var
  I: Cardinal;
begin
  { Call upper constructor }
  Create(APriorityType, AValueType, DefaultArrayLength, Ascending);

  { Copy all items in }
  if AArray.Length > 0 then
    for I := 0 to AArray.Length - 1 do
      Enqueue(AArray[I].Value, AArray[I].Key);
end;

constructor TPriorityQueue<TPriority, TValue>.Create(
  const APriorityType: IType<TPriority>;
  const AValueType: IType<TValue>;
  const InitialCapacity: Cardinal;
  const Ascending: Boolean);
begin
  { Initialize instance }
  if (APriorityType = nil) then
     ExceptionHelper.Throw_ArgumentNilError('APriorityType');

  if (AValueType = nil) then
     ExceptionHelper.Throw_ArgumentNilError('AValueType');

  { Install types }
  InstallTypes(APriorityType, AValueType);

  SetLength(FArray, InitialCapacity);
  FVer := 0;
  FCount := 0;

  if Ascending then
    FSign := 1
  else
    FSign := -1;
end;

constructor TPriorityQueue<TPriority, TValue>.Create(const InitialCapacity: Cardinal;
  const Ascending: Boolean);
begin
  { Call upper constructor }
  Create(TType<TPriority>.Default, TType<TValue>.Default, InitialCapacity, Ascending);
end;

constructor TPriorityQueue<TPriority, TValue>.Create(
  const APriorityType: IType<TPriority>;
  const AValueType: IType<TValue>;
  const AEnumerable: IEnumerable<TKeyValuePair<TPriority, TValue>>;
  const Ascending: Boolean);
var
  V: TKeyValuePair<TPriority, TValue>;
begin
  { Call upper constructor }
  Create(APriorityType, AValueType, DefaultArrayLength, Ascending);

  if (AEnumerable = nil) then
     ExceptionHelper.Throw_ArgumentNilError('AEnumerable');

  { Pump in all items }
  for V in AEnumerable do
    Enqueue(V.Value, V.Key);
end;

constructor TPriorityQueue<TPriority, TValue>.Create(
  const APriorityType: IType<TPriority>;
  const AValueType: IType<TValue>;
  const Ascending: Boolean);
begin
  { Call upper constructor }
  Create(APriorityType, AValueType, DefaultArrayLength, Ascending);
end;

constructor TPriorityQueue<TPriority, TValue>.Create(const AArray: TFixedArray<TKeyValuePair<TPriority, TValue>>;
  const Ascending: Boolean);
begin
  { Call upper constructor }
  Create(TType<TPriority>.Default, TType<TValue>.Default, AArray, Ascending);
end;

function TPriorityQueue<TPriority, TValue>.Dequeue: TValue;
var
  LPair: TPriorityPair;
begin
  if FCount = 0 then
    ExceptionHelper.Throw_CollectionEmptyError();

  { Extract element at position zero (the head) }
  LPair := RemoveAt(0);

  { CLeanup the priority element }
  if KeyType.Management = tmManual then
    KeyType.Cleanup(LPair.FPriority);

  { And return the value }
  Result := LPair.FValue;
  Inc(FVer);
end;

procedure TPriorityQueue<TPriority, TValue>.DeserializePair(const AKey: TPriority; const AValue: TValue);
begin
  { Simple as hell ... }
  Enqueue(AValue, AKey);
end;

destructor TPriorityQueue<TPriority, TValue>.Destroy;
begin
  { First clear }
  Clear();

  inherited;
end;

procedure TPriorityQueue<TPriority, TValue>.Enqueue(const AValue: TValue; const APriority: TPriority);
var
  I, X: Cardinal;
begin
  { Grow if required }
  if FCount = Cardinal(Length(FArray)) then
    Grow();

  I := FCount;
  Inc(FCount);

  { Move items to new positions }
  while true do
  begin
    if I > 0 then
      X := (I - 1) div 2
    else
      X := 0;

    { Check for exit }
    if (I = 0) or ((KeyType.Compare(FArray[X].FPriority, APriority) * FSign) > 0) then
      break;

    FArray[I] := FArray[X];
    I := X;
  end;

  { Insert the new item }
  FArray[I].FPriority := APriority;
  FArray[I].FValue := AValue;

  Inc(FVer);
end;

procedure TPriorityQueue<TPriority, TValue>.Enqueue(const AValue: TValue);
begin
  { Insert with default priority }
  Enqueue(AValue, default(TPriority));
end;

function TPriorityQueue<TPriority, TValue>.GetCapacity: Cardinal;
begin
  Result := Length(FArray);
end;

function TPriorityQueue<TPriority, TValue>.GetCount: Cardinal;
begin
  { Use the FCount }
  Result := FCount;
end;

function TPriorityQueue<TPriority, TValue>.GetEnumerator: IEnumerator<TKeyValuePair<TPriority, TValue>>;
begin
  { Create an enumerator }
  Result := TPairEnumerator.Create(Self);
end;

procedure TPriorityQueue<TPriority, TValue>.Grow;
var
  LNewCapacity: Cardinal;
begin
  LNewCapacity := Length(FArray) * 2;

  if LNewCapacity < DefaultArrayLength then
    LNewCapacity := DefaultArrayLength;

  { Extend the array }
  SetLength(FArray, LNewCapacity);
end;

function TPriorityQueue<TPriority, TValue>.MaxKey: TPriority;
begin
  if FCount = 0 then
    ExceptionHelper.Throw_CollectionEmptyError();

  Result := FArray[0].FPriority;
end;

function TPriorityQueue<TPriority, TValue>.Peek: TValue;
begin
  if FCount = 0 then
    ExceptionHelper.Throw_CollectionEmptyError();

  { Peek at the element at position zero (the head) }
  Result := FArray[0].FValue;
end;

function TPriorityQueue<TPriority, TValue>.RemoveAt(const AIndex: Cardinal): TPriorityPair;
var
  LTemp: TPriorityPair;
  I, X, LStart: Cardinal;
begin
  { Obtain the item that is removed }
  Result := FArray[AIndex];
  LTemp := FArray[FCount - 1];

  Dec(FCount);

  { Fill in the create hole }
  if (FCount = 0) or (AIndex = FCount) then
    Exit;

  I := AIndex;

  if I > 0 then
    LStart := (I - 1) div 2
  else
    LStart := 0;

  while ((KeyType.Compare(LTemp.FPriority, FArray[LStart].FPriority) * FSign) > 0) do
  begin
    FArray[I] := FArray[LStart];
    I := LStart;

    if I > 0 then
      LStart := (I - 1) div 2
    else
      LStart := 0;
  end;

  if (I = AIndex) then
  begin
    while (I < (FCount div 2)) do
    begin
      X := (I * 2) + 1;

      if ((X < FCount - 1) and ((KeyType.Compare(FArray[X].FPriority, FArray[X + 1].FPriority) * FSign) < 0)) then
        Inc(X);

      if ((KeyType.Compare(FArray[X].FPriority, LTemp.FPriority) * FSign) <= 0) then
          break;

      FArray[I] := FArray[X];
      I := X;
    end;
  end;

  FArray[I] := LTemp;
end;

procedure TPriorityQueue<TPriority, TValue>.Shrink;
begin
  { Remove the excess stuff }
  if FCount < Cardinal(Length(FArray)) then
    SetLength(FArray, FCount);
end;

procedure TPriorityQueue<TPriority, TValue>.StartDeserializing(const AData: TDeserializationData);
var
  LAsc: Boolean;
begin
  { Try to obtain the ascending sign }
  AData.GetValue(SSerAscendingKeys, LAsc);

  { Call the constructor in this instance to initialize myself first }
  Create(LAsc);
end;

procedure TPriorityQueue<TPriority, TValue>.StartSerializing(const AData: TSerializationData);
begin
  { Write the ascending sign }
  AData.AddValue(SSerAscendingKeys, (FSign = 1));
end;

{ TPriorityQueue<TPriority, TValue>.TPairEnumerator }

constructor TPriorityQueue<TPriority, TValue>.TPairEnumerator.Create(const AQueue: TPriorityQueue<TPriority, TValue>);
begin
  FQueue := AQueue;
  KeepObjectAlive(FQueue);

  FVer := AQueue.FVer;
  FCurrentIndex := 0;
end;

destructor TPriorityQueue<TPriority, TValue>.TPairEnumerator.Destroy;
begin
  ReleaseObject(FQueue);
  inherited;
end;

function TPriorityQueue<TPriority, TValue>.TPairEnumerator.GetCurrent: TKeyValuePair<TPriority, TValue>;
begin
  if FVer <> FQueue.FVer then
     ExceptionHelper.Throw_CollectionChangedError();

  if FCurrentIndex > 0 then
    Result := TKeyValuePair<TPriority, TValue>.Create(
      FQueue.FArray[FCurrentIndex - 1].FPriority, FQueue.FArray[FCurrentIndex - 1].FValue)
  else
    Result := default(TKeyValuePair<TPriority, TValue>);
end;

function TPriorityQueue<TPriority, TValue>.TPairEnumerator.MoveNext: Boolean;
begin
  if FVer <> FQueue.FVer then
     ExceptionHelper.Throw_CollectionChangedError();

  Result := FCurrentIndex < FQueue.FCount;
  Inc(FCurrentIndex);
end;

{ TObjectPriorityQueue<TPriority, TValue> }

procedure TObjectPriorityQueue<TPriority, TValue>.InstallTypes(const AKeyType: IType<TPriority>; const AValueType: IType<TValue>);
begin
  { Create a wrapper over the real type class and switch it }
  FPriorityWrapperType := TObjectWrapperType<TPriority>.Create(AKeyType);
  FValueWrapperType := TObjectWrapperType<TValue>.Create(AValueType);

  { Install overridden type }
  inherited InstallTypes(FPriorityWrapperType, FValueWrapperType);
end;

function TObjectPriorityQueue<TPriority, TValue>.GetOwnsPriorities: Boolean;
begin
  Result := FPriorityWrapperType.AllowCleanup;
end;

function TObjectPriorityQueue<TPriority, TValue>.GetOwnsValues: Boolean;
begin
  Result := FValueWrapperType.AllowCleanup;
end;

procedure TObjectPriorityQueue<TPriority, TValue>.SetOwnsPriorities(const Value: Boolean);
begin
  FPriorityWrapperType.AllowCleanup := Value;
end;

procedure TObjectPriorityQueue<TPriority, TValue>.SetOwnsValues(const Value: Boolean);
begin
  FValueWrapperType.AllowCleanup := Value;
end;

end.
