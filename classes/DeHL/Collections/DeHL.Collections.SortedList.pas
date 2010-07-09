(*
* Copyright (c) 2008-2009, Lucian Bentea
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
unit DeHL.Collections.SortedList;
interface
uses SysUtils,
     DeHL.Base,
     DeHL.Types,
     DeHL.Exceptions,
     DeHL.Arrays,
     DeHL.StrConsts,
     DeHL.Serialization,     
     DeHL.Collections.Base;

type
  { Generic List }
  TSortedList<T> = class(TEnexCollection<T>, ISortedList<T>, IList<T>, IEnexIndexedCollection<T>, IDynamic)
  private
  type
    { Generic List Enumerator }
    TEnumerator = class(TEnumerator<T>)
    private
      FVer         : Cardinal;
      FList        : TSortedList<T>;
      FCurrentIndex: Cardinal;

    public
      { Constructor }
      constructor Create(const AList: TSortedList<T>);

      { Destructor }
      destructor Destroy(); override;

      function GetCurrent(): T; override;
      function MoveNext(): Boolean; override;
    end;

  var
    FArray    : TArray<T>;
    FLength   : Cardinal;
    FVer      : Cardinal;
    FAscending: Boolean;

     { Internal insertion }
     procedure Insert(const AtIndex: Cardinal; const AValue: T);
  protected
    { Serialization overrides }
    procedure StartSerializing(const AData: TSerializationData); override;
    procedure StartDeserializing(const AData: TDeserializationData); override;
    procedure DeserializeElement(const AElement: T); override;

    { IList: Getter/Setter functions }
    function GetItem(const Index: Cardinal): T;

    { ICollection support/hidden }
    function GetCount(): Cardinal; override;

    { Gets the current capacity of the collection }
    function GetCapacity(): Cardinal;
  public
    { Constructors }
    constructor Create(const Ascending: Boolean = true); overload;
    constructor Create(const InitialCapacity: Cardinal; const Ascending: Boolean = true); overload;
    constructor Create(const AEnumerable: IEnumerable<T>; const Ascending: Boolean = true); overload;
    constructor Create(const AArray: array of T; const Ascending: Boolean = true); overload;
    constructor Create(const AArray: TDynamicArray<T>; const Ascending: Boolean = true); overload;
    constructor Create(const AArray: TFixedArray<T>; const Ascending: Boolean = true); overload;

    constructor Create(const AType: IType<T>; const Ascending: Boolean = true); overload;
    constructor Create(const AType: IType<T>; const InitialCapacity: Cardinal; const Ascending: Boolean = true); overload;
    constructor Create(const AType: IType<T>; const AEnumerable: IEnumerable<T>; const Ascending: Boolean = true); overload;
    constructor Create(const AType: IType<T>; const AArray: array of T; const Ascending: Boolean = true); overload;
    constructor Create(const AType: IType<T>; const AArray: TDynamicArray<T>; const Ascending: Boolean = true); overload;
    constructor Create(const AType: IType<T>; const AArray: TFixedArray<T>; const Ascending: Boolean = true); overload;

    { Destructor }
    destructor Destroy(); override;

    { Modification }
    procedure Clear();

    procedure Add(const AValue: T); overload;
    procedure Add(const AEnumerable: IEnumerable<T>); overload;

    procedure Remove(const AValue: T);
    procedure RemoveAt(const AtIndex: Cardinal);

    { Look-up }
    function Contains(const AValue: T): Boolean;

    function IndexOf(const AValue: T; const StartIndex, Count: Cardinal): Integer; overload;
    function IndexOf(const AValue: T; const StartIndex: Cardinal): Integer; overload;
    function IndexOf(const AValue: T): Integer; overload;

    function LastIndexOf(const AValue: T; const StartIndex, Count: Cardinal): Integer; overload;
    function LastIndexOf(const AValue: T; const StartIndex: Cardinal): Integer; overload;
    function LastIndexOf(const AValue: T): Integer; overload;

    { Others }
    property Count: Cardinal read FLength;
    property Items[const Index: Cardinal]: T read GetItem; default;
    property Capacity: Cardinal read GetCapacity;

    { Grow/Shrink }
    procedure Shrink();
    procedure Grow();

    { IEnumerable/ ICollection support }
    function GetEnumerator(): IEnumerator<T>; override;

    { Copy-To }
    procedure CopyTo(var AArray: array of T); overload; override;
    procedure CopyTo(var AArray: array of T; const StartIndex: Cardinal); overload; override;

    function Copy(const StartIndex: Cardinal; const Count: Cardinal): TSortedList<T>; overload;
    function Copy(const StartIndex: Cardinal): TSortedList<T>; overload;
    function Copy(): TSortedList<T>; overload;

    { Enex Overrides }
    function Empty(): Boolean; override;
    function Max(): T; override;
    function Min(): T; override;
    function First(): T; override;
    function FirstOrDefault(const ADefault: T): T; override;
    function Last(): T; override;
    function LastOrDefault(const ADefault: T): T; override;
    function Single(): T; override;
    function SingleOrDefault(const ADefault: T): T; override;
    function Aggregate(const AAggregator: TFunc<T, T, T>): T; override;
    function AggregateOrDefault(const AAggregator: TFunc<T, T, T>; const ADefault: T): T; override;
    function ElementAt(const Index: Cardinal): T; override;
    function ElementAtOrDefault(const Index: Cardinal; const ADefault: T): T; override;
    function Any(const APredicate: TFunc<T, Boolean>): Boolean; override;
    function All(const APredicate: TFunc<T, Boolean>): Boolean; override;
    function EqualsTo(const AEnumerable: IEnumerable<T>): Boolean; override;
  end;

  { The object variant }
  TObjectSortedList<T: class> = class sealed(TSortedList<T>)
  private
    FWrapperType: TObjectWrapperType<T>;

    { Getters/Setters for OwnsObjects }
    function GetOwnsObjects: Boolean;
    procedure SetOwnsObjects(const Value: Boolean);

  protected
    { Override in descendants to support proper stuff }
    procedure InstallType(const AType: IType<T>); override;

  public

    { Object owning }
    property OwnsObjects: Boolean read GetOwnsObjects write SetOwnsObjects;
  end;

implementation

const
  DefaultArrayLength = 32;

{ TSortedList<T> }

procedure TSortedList<T>.Insert(const AtIndex: Cardinal; const AValue: T);
var
  I  : Integer;
  Cap: Cardinal;
begin
  if AtIndex > FLength then
     ExceptionHelper.Throw_ArgumentOutOfRangeError('AtIndex');

  if FLength = Cardinal(Length(FArray)) then
    Grow();

  { Move the array to the right }
  if AtIndex < FLength then
     for I := FLength downto (AtIndex + 1) do
         FArray[I] := FArray[I - 1];

  Inc(FLength);

  { Put the element into the new position }
  FArray[AtIndex] := AValue;
  Inc(FVer);
end;

procedure TSortedList<T>.Add(const AEnumerable: IEnumerable<T>);
var
  V: T;
begin
  if (AEnumerable = nil) then
     ExceptionHelper.Throw_ArgumentNilError('AEnumerable');

  { Enumerate and add, preserving order}
  for V in AEnumerable do
    Add(V);
end;

function TSortedList<T>.Aggregate(const AAggregator: TFunc<T, T, T>): T;
var
  I: Cardinal;
begin
  { Check arguments }
  if not Assigned(AAggregator) then
    ExceptionHelper.Throw_ArgumentNilError('AAggregator');

  if FLength = 0 then
    ExceptionHelper.Throw_CollectionEmptyError();

  { Select the first element as comparison base }
  Result := FArray[0];

  { Iterate over the last N - 1 elements }
  for I := 1 to FLength - 1 do
  begin
    { Aggregate a value }
    Result := AAggregator(Result, FArray[I]);
  end;
end;

function TSortedList<T>.AggregateOrDefault(const AAggregator: TFunc<T, T, T>; const ADefault: T): T;
var
  I: Cardinal;
begin
  { Check arguments }
  if not Assigned(AAggregator) then
    ExceptionHelper.Throw_ArgumentNilError('AAggregator');

  if FLength = 0 then
    Exit(ADefault);

  { Select the first element as comparison base }
  Result := FArray[0];

  { Iterate over the last N - 1 elements }
  for I := 1 to FLength - 1 do
  begin
    { Aggregate a value }
    Result := AAggregator(Result, FArray[I]);
  end;
end;

function TSortedList<T>.All(const APredicate: TFunc<T, Boolean>): Boolean;
var
  I: Cardinal;
begin
  if not Assigned(APredicate) then
    ExceptionHelper.Throw_ArgumentNilError('APredicate');

  if FLength > 0 then
    for I := 0 to FLength - 1 do
      if not APredicate(FArray[I]) then
        Exit(false);

  Result := true;
end;

function TSortedList<T>.Any(const APredicate: TFunc<T, Boolean>): Boolean;
var
  I: Cardinal;
begin
  if not Assigned(APredicate) then
    ExceptionHelper.Throw_ArgumentNilError('APredicate');

  if FLength > 0 then
    for I := 0 to FLength - 1 do
      if APredicate(FArray[I]) then
        Exit(true);

  Result := false;
end;

procedure TSortedList<T>.Add(const AValue: T);
var
  I: Cardinal;
  Sign: Integer;
begin

  if FAscending then
     Sign := 1
  else
     Sign := -1;

  I := 0;

  while I < FLength do
  begin
    if ((ElementType.Compare(AValue, FArray[I]) * Sign) < 0) then
       Break;

    Inc(I);
  end;

  Insert(I, AValue);
end;

procedure TSortedList<T>.Clear;
var
  I: Integer;
begin
  if (ElementType <> nil) and (ElementType.Management() = tmManual) and (FLength > 0) then
  begin
    { Should cleanup each element individually }
    for I := 0 to FLength - 1 do
      ElementType.Cleanup(FArray[I]);
  end;

  { Reset the length }
  FLength := 0;
end;

function TSortedList<T>.Contains(const AValue: T): Boolean;
begin
  { Pass the call to index of }
  Result := (IndexOf(AValue) > -1);
end;

procedure TSortedList<T>.CopyTo(var AArray: array of T);
begin
  { Call the more generic copy to }
  CopyTo(AArray, 0);
end;

function TSortedList<T>.Copy(const StartIndex: Cardinal): TSortedList<T>;
begin
  { Pass the call down to the more generic function }
  Copy(StartIndex, (FLength - StartIndex));
end;

function TSortedList<T>.Copy(const StartIndex, Count: Cardinal): TSortedList<T>;
var
  NewList: TSortedList<T>;

begin
  { Check for zero elements }
  if (FLength = 0) then
  begin
    Result := TSortedList<T>.Create(ElementType);
    Exit;
  end;

  { Check for indexes }
  if (StartIndex >= FLength) then
     ExceptionHelper.Throw_ArgumentOutOfRangeError('StartIndex');

  { Check for indexes }
  if ((StartIndex + Count) > FLength) then
     ExceptionHelper.Throw_ArgumentOutOfRangeError('Count');

  { Create a new list }
  NewList := TSortedList<T>.Create(ElementType, Count);

  { Copy all elements safely }
  &Array<T>.SafeMove(FArray, NewList.FArray, StartIndex, 0, Count, ElementType);

  { Set new count }
  NewList.FLength := Count;

  Result := NewList;
end;

procedure TSortedList<T>.CopyTo(var AArray: array of T; const StartIndex: Cardinal);
begin
  { Check for indexes }
  if StartIndex >= Cardinal(Length(AArray)) then
    ExceptionHelper.Throw_ArgumentOutOfRangeError('StartIndex');

  if (Cardinal(Length(AArray)) - StartIndex) < FLength then
     ExceptionHelper.Throw_ArgumentOutOfSpaceError('AArray');

  { Copy all elements safely }
  &Array<T>.SafeMove(FArray, AArray, 0, StartIndex, FLength, ElementType);
end;

constructor TSortedList<T>.Create(const AType: IType<T>; const Ascending: Boolean);
begin
  { Call upper constructor }
  Create(AType, DefaultArrayLength, Ascending);
end;

constructor TSortedList<T>.Create(const AType: IType<T>;
  const AEnumerable: IEnumerable<T>; const Ascending: Boolean);
var
  V: T;
begin
  { Call upper constructor }
  Create(AType, DefaultArrayLength, Ascending);

  { Initialize instance }
  if (AEnumerable = nil) then
     ExceptionHelper.Throw_ArgumentNilError('AEnumerable');

  { Try to copy the given Enumerable }
  for V in AEnumerable do
  begin
    { Perform a simple push }
    Add(V);
  end;
end;

constructor TSortedList<T>.Create(const Ascending: Boolean);
begin
  Create(TType<T>.Default, Ascending);
end;

constructor TSortedList<T>.Create(const InitialCapacity: Cardinal; const Ascending: Boolean);
begin
  Create(TType<T>.Default, InitialCapacity, Ascending);
end;

constructor TSortedList<T>.Create(const AEnumerable: IEnumerable<T>; const Ascending: Boolean);
begin
  Create(TType<T>.Default, AEnumerable, Ascending);
end;

constructor TSortedList<T>.Create(const AType: IType<T>;
  const InitialCapacity: Cardinal; const Ascending: Boolean);
begin
  { Initialize instance }
  if (AType = nil) then
     ExceptionHelper.Throw_ArgumentNilError('AType');

  InstallType(AType);

  FLength := 0;
  FVer := 0;
  FAscending := Ascending;

  SetLength(FArray, InitialCapacity);

  { Call the default (Thread Safety) constructor }
  inherited Create();
end;

procedure TSortedList<T>.DeserializeElement(const AElement: T);
begin
  { Simple as hell ... }
  Add(AElement);
end;

destructor TSortedList<T>.Destroy;
begin
  { Clear list first }
  Clear();

  inherited;
end;

function TSortedList<T>.ElementAt(const Index: Cardinal): T;
begin
  { Simply use the getter }
  Result := GetItem(Index);
end;

function TSortedList<T>.ElementAtOrDefault(const Index: Cardinal; const ADefault: T): T;
begin
  { Check range }
  if (Index >= FLength) then
     Result := ADefault
  else
    Result := FArray[Index];
end;

function TSortedList<T>.Empty: Boolean;
begin
  Result := (FLength = 0);
end;

function TSortedList<T>.EqualsTo(const AEnumerable: IEnumerable<T>): Boolean;
var
  V: T;
  I: Cardinal;
begin
  I := 0;

  for V in AEnumerable do
  begin
    if I >= FLength then
      Exit(false);

    if not ElementType.AreEqual(FArray[I], V) then
      Exit(false);

    Inc(I);
  end;

  if I < FLength then
    Exit(false);

  Result := true;
end;

function TSortedList<T>.First: T;
begin
  { Check length }
  if FLength = 0 then
    ExceptionHelper.Throw_CollectionEmptyError();

  Result := FArray[0];
end;

function TSortedList<T>.FirstOrDefault(const ADefault: T): T;
begin
  { Check length }
  if FLength = 0 then
    Result := ADefault
  else
    Result := FArray[0];
end;

function TSortedList<T>.GetCapacity: Cardinal;
begin
  Result := Length(FArray);
end;

function TSortedList<T>.GetCount: Cardinal;
begin
  Result := FLength;
end;

function TSortedList<T>.GetEnumerator: IEnumerator<T>;
begin
  { Create an enumerator }
  Result := TEnumerator.Create(Self);
end;

function TSortedList<T>.GetItem(const Index: Cardinal): T;
begin
  { Check range }
  if (Index >= FLength) then
     ExceptionHelper.Throw_ArgumentOutOfRangeError('Index');

  { Get value }
  Result := FArray[Index];
end;

procedure TSortedList<T>.Grow;
begin
  { Grow the array }
  if FLength < DefaultArrayLength then
     SetLength(FArray, FLength + DefaultArrayLength)
  else
     SetLength(FArray, FLength * 2);
end;

function TSortedList<T>.IndexOf(const AValue: T): Integer;
begin
  { Call more generic function }
  Result := IndexOf(AValue, 0, FLength);
end;

function TSortedList<T>.IndexOf(const AValue: T;
  const StartIndex: Cardinal): Integer;
begin
  { Call more generic function }
  Result := IndexOf(AValue, StartIndex, (FLength - StartIndex));
end;

function TSortedList<T>.IndexOf(const AValue: T; const StartIndex,
  Count: Cardinal): Integer;
var
  I, J: Integer;
begin
  Result := -1;

  if FLength = 0 then
     Exit;

  { Check for indexes }
  if (StartIndex >= FLength) then
     ExceptionHelper.Throw_ArgumentOutOfRangeError('StartIndex');

  { Check for indexes }
  if ((StartIndex + Count) > FLength) then
     ExceptionHelper.Throw_ArgumentOutOfRangeError('Count');

  { Search for the value }
  J := &Array<T>.BinarySearch(FArray, AValue, StartIndex, Count, ElementType, FAscending);

  if J = -1 then
     Exit(-1)
  else
    Inc(J, StartIndex);

  for I := J - 1 downto StartIndex do
      if not ElementType.AreEqual(AValue, FArray[I]) then
      begin
        Result := I + 1;
        Exit;
      end;
  Result := J;
end;

function TSortedList<T>.LastIndexOf(const AValue: T;
  const StartIndex: Cardinal): Integer;
begin
  { Call more generic function }
  Result := LastIndexOf(AValue, StartIndex, (FLength - StartIndex));
end;

function TSortedList<T>.Last: T;
begin
  { Check length }
  if FLength = 0 then
    ExceptionHelper.Throw_CollectionEmptyError();

  Result := FArray[FLength - 1];
end;

function TSortedList<T>.LastIndexOf(const AValue: T): Integer;
begin
  { Call more generic function }
  Result := LastIndexOf(AValue, 0, FLength);
end;

function TSortedList<T>.LastOrDefault(const ADefault: T): T;
begin
  { Check length }
  if FLength = 0 then
    Result := ADefault
  else
    Result := FArray[FLength - 1];
end;

function TSortedList<T>.Max: T;
var
  I: Cardinal;
begin
  { Check length }
  if FLength = 0 then
    ExceptionHelper.Throw_CollectionEmptyError();

  { Default one }
  Result := FArray[0];

  for I := 1 to FLength - 1 do
    if ElementType.Compare(FArray[I], Result) > 0 then
      Result := FArray[I];
end;

function TSortedList<T>.Min: T;
var
  I: Cardinal;
begin
  { Check length }
  if FLength = 0 then
    ExceptionHelper.Throw_CollectionEmptyError();

  { Default one }
  Result := FArray[0];

  for I := 1 to FLength - 1 do
    if ElementType.Compare(FArray[I], Result) < 0 then
      Result := FArray[I];
end;

function TSortedList<T>.LastIndexOf(const AValue: T; const StartIndex,
  Count: Cardinal): Integer;
var
  I, J: Integer;
begin
  Result := -1;

  if FLength = 0 then
     Exit;

  { Check for indexes }
  if (StartIndex >= FLength) then
     ExceptionHelper.Throw_ArgumentOutOfRangeError('StartIndex');

  { Check for indexes }
  if ((StartIndex + Count) > FLength) then
     ExceptionHelper.Throw_ArgumentOutOfRangeError('Count');

  { Search for the value }
  J :=  &Array<T>.BinarySearch(FArray, AValue, StartIndex, Count, ElementType, FAscending);

  if J = -1 then
     Exit(-1)
  else
    Inc(J, StartIndex);

  for I := J + 1 to StartIndex + Count - 1 do
    if not ElementType.AreEqual(AValue, FArray[I]) then
    begin
      Result := I - 1;
      Exit;
    end;

  Result := J;
end;

procedure TSortedList<T>.Remove(const AValue: T);
var
  I, FoundIndex: Integer;
begin
  { Defaults }
  if (FLength = 0) then Exit;
  FoundIndex := -1;

  for I := 0 to FLength - 1 do
  begin
    if ElementType.AreEqual(FArray[I], AValue) then
    begin
      FoundIndex := I;
      Break;
    end;
  end;

  if FoundIndex > -1 then
  begin
    { Move the list }
    if FLength > 1 then
      for I := FoundIndex to FLength - 2 do
        FArray[I] := FArray[I + 1];

    Dec(FLength);
    Inc(FVer);
  end;
end;

procedure TSortedList<T>.RemoveAt(const AtIndex: Cardinal);
var
  I: Integer;
begin
  if AtIndex >= FLength then
     ExceptionHelper.Throw_ArgumentOutOfRangeError('AtIndex');

  if (FLength = 0) then Exit;

  { Clanup the element at the specified index if required }
  if ElementType.Management() = tmManual then
    ElementType.Cleanup(FArray[AtIndex]);

  { Move the list }
  if FLength > 1 then
    for I := AtIndex to FLength - 2 do
      FArray[I] := FArray[I + 1];

  Dec(FLength);
  Inc(FVer);
end;

procedure TSortedList<T>.Shrink;
begin
  { Cut the capacity if required }
  if FLength < Capacity then
  begin
    SetLength(FArray, FLength);
  end;
end;

function TSortedList<T>.Single: T;
begin
  { Check length }
  if FLength = 0 then
    ExceptionHelper.Throw_CollectionEmptyError()
  else if FLength > 1 then
    ExceptionHelper.Throw_CollectionHasMoreThanOneElement()
  else
    Result := FArray[0];
end;

function TSortedList<T>.SingleOrDefault(const ADefault: T): T;
begin
  { Check length }
  if FLength = 0 then
    Result := ADefault
  else if FLength > 1 then
    ExceptionHelper.Throw_CollectionHasMoreThanOneElement()
  else
    Result := FArray[0];
end;

procedure TSortedList<T>.StartDeserializing(const AData: TDeserializationData);
var
  LAsc: Boolean;
begin
  AData.GetValue(SSerAscendingKeys, LAsc);

  { Call the constructor in this instance to initialize myself first }
  Create(LAsc);
end;

procedure TSortedList<T>.StartSerializing(const AData: TSerializationData);
begin
  { Write the ascending sign }
  AData.AddValue(SSerAscendingKeys, FAscending);
end;

function TSortedList<T>.Copy: TSortedList<T>;
begin
  { Call a more generic function }
  Result := Copy(0, FLength);
end;

constructor TSortedList<T>.Create(const AArray: array of T; const Ascending: Boolean);
begin
  Create(TType<T>.Default, AArray, Ascending);
end;

constructor TSortedList<T>.Create(const AType: IType<T>; const AArray: array of T; const Ascending: Boolean);
var
  I: Integer;
begin
  { Call upper constructor }
  Create(AType, DefaultArrayLength, Ascending);

  { Copy from array }
  for I := 0 to Length(AArray) - 1 do
  begin
    Add(AArray[I]);
  end;
end;

constructor TSortedList<T>.Create(const AArray: TFixedArray<T>; const Ascending: Boolean);
begin
  Create(TType<T>.Default, AArray, Ascending);
end;

constructor TSortedList<T>.Create(const AArray: TDynamicArray<T>; const Ascending: Boolean);
begin
  Create(TType<T>.Default, AArray, Ascending);
end;

constructor TSortedList<T>.Create(const AType: IType<T>; const AArray: TFixedArray<T>; const Ascending: Boolean);
var
  I: Cardinal;
begin
  { Call upper constructor }
  Create(AType, DefaultArrayLength, Ascending);

  { Copy from array }
  if AArray.Length > 0 then
    for I := 0 to AArray.Length - 1 do
    begin
      Add(AArray[I]);
    end;
end;

constructor TSortedList<T>.Create(const AType: IType<T>; const AArray: TDynamicArray<T>; const Ascending: Boolean);
var
  I: Cardinal;
begin
  { Call upper constructor }
  Create(AType, DefaultArrayLength, Ascending);

  { Copy from array }
  if AArray.Length > 0 then
    for I := 0 to AArray.Length - 1 do
    begin
      Add(AArray[I]);
    end;
end;

{ TSortedList<T>.TEnumerator }

constructor TSortedList<T>.TEnumerator.Create(const AList: TSortedList<T>);
begin
  { Initialize }
  FList := AList;
  KeepObjectAlive(FList);

  FCurrentIndex := 0;
  FVer := FList.FVer;
end;

destructor TSortedList<T>.TEnumerator.Destroy;
begin
  ReleaseObject(FList);
  inherited;
end;

function TSortedList<T>.TEnumerator.GetCurrent: T;
begin
  if FVer <> FList.FVer then
     ExceptionHelper.Throw_CollectionChangedError();

  if FCurrentIndex > 0 then
    Result := FList.FArray[FCurrentIndex - 1]
  else
    Result := default(T);
end;

function TSortedList<T>.TEnumerator.MoveNext: Boolean;
begin
  if FVer <> FList.FVer then
     ExceptionHelper.Throw_CollectionChangedError();

  Result := FCurrentIndex < FList.FLength;
  Inc(FCurrentIndex);
end;

{ TObjectSortedList<T> }

procedure TObjectSortedList<T>.InstallType(const AType: IType<T>);
begin
  { Create a wrapper over the real type class and switch it }
  FWrapperType := TObjectWrapperType<T>.Create(AType);

  { Install overridden type }
  inherited InstallType(FWrapperType);
end;

function TObjectSortedList<T>.GetOwnsObjects: Boolean;
begin
  Result := FWrapperType.AllowCleanup;
end;

procedure TObjectSortedList<T>.SetOwnsObjects(const Value: Boolean);
begin
  FWrapperType.AllowCleanup := Value;
end;

end.
