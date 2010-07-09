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
unit DeHL.Collections.List;
interface
uses SysUtils,
     DeHL.Base,
     DeHL.Types,
     DeHL.Exceptions,
     DeHL.Arrays,
     DeHL.Serialization,
     DeHL.Collections.Base;

type
  { Generic List }
  TList<T> = class(TEnexCollection<T>, IList<T>, IEnexIndexedCollection<T>, IDynamic)
  private
  type
    { Generic List Enumerator }
    TEnumerator = class(TEnumerator<T>)
    private
      FVer         : Cardinal;
      FList        : TList<T>;
      FCurrentIndex: Cardinal;

    public
      { Constructor }
      constructor Create(const AList: TList<T>);

      { Destructor }
      destructor Destroy(); override;

      function GetCurrent(): T; override;
      function MoveNext(): Boolean; override;
    end;

  var
    FArray : TArray<T>;
    FLength: Cardinal;
    FVer   : Cardinal;

  protected
    { Serialization overrides }
    procedure StartSerializing(const AData: TSerializationData); override;
    procedure StartDeserializing(const AData: TDeserializationData); override;
    procedure DeserializeElement(const AElement: T); override;

    { IList: Getter/Setter functions }
    function GetItem(const Index: Cardinal): T;
    procedure SetItem(const Index: Cardinal; const Value: T);

    { ICollection support/hidden }
    function GetCount(): Cardinal; override;

    { Gets the current capacity of the collection }
    function GetCapacity(): Cardinal;
  public
    { Constructors }
    constructor Create(); overload;
    constructor Create(const InitialCapacity: Cardinal); overload;
    constructor Create(const AEnumerable: IEnumerable<T>); overload;
    constructor Create(const AArray: array of T); overload;
    constructor Create(const AArray: TDynamicArray<T>); overload;
    constructor Create(const AArray: TFixedArray<T>); overload;

    constructor Create(const AType: IType<T>); overload;
    constructor Create(const AType: IType<T>; const InitialCapacity: Cardinal); overload;
    constructor Create(const AType: IType<T>; const AEnumerable: IEnumerable<T>); overload;
    constructor Create(const AType: IType<T>; const AArray: array of T); overload;
    constructor Create(const AType: IType<T>; const AArray: TDynamicArray<T>); overload;
    constructor Create(const AType: IType<T>; const AArray: TFixedArray<T>); overload;

    { Destructor }
    destructor Destroy(); override;

    { Modification }
    procedure Clear();

    procedure Add(const AValue: T); overload;
    procedure Add(const AEnumerable: IEnumerable<T>); overload;

    procedure Insert(const AtIndex: Cardinal; const AValue: T); overload;
    procedure Insert(const AtIndex: Cardinal; const AEnumerable: IEnumerable<T>); overload;

    procedure Remove(const AValue: T);
    procedure RemoveAt(const AtIndex: Cardinal);

    { Operations }
    procedure Reverse(const StartIndex, Count: Cardinal); overload;
    procedure Reverse(const StartIndex: Cardinal); overload;
    procedure Reverse(); overload;

    procedure Sort(const StartIndex, Count: Cardinal; const Ascending: Boolean = true); overload;
    procedure Sort(const StartIndex: Cardinal; const Ascending: Boolean = true); overload;
    procedure Sort(const Ascending: Boolean = true); overload;

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
    property Capacity: Cardinal read GetCapacity;
    property Items[const Index: Cardinal]: T read GetItem write SetItem; default;

    { IEnumerable/ ICollection support }
    function GetEnumerator(): IEnumerator<T>; override;

    { Grow/Shrink }
    procedure Shrink();
    procedure Grow();

    { Copy-To }
    procedure CopyTo(var AArray: array of T); overload; override;
    procedure CopyTo(var AArray: array of T; const StartIndex: Cardinal); overload; override;

    function Copy(const StartIndex: Cardinal; const Count: Cardinal): TList<T>; overload;
    function Copy(const StartIndex: Cardinal): TList<T>; overload;
    function Copy(): TList<T>; overload;

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
  TObjectList<T: class> = class(TList<T>)
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

{ TList<T> }

procedure TList<T>.Add(const AEnumerable: IEnumerable<T>);
begin
  { Call Insert }
  Insert(FLength, AEnumerable);
end;

function TList<T>.Aggregate(const AAggregator: TFunc<T, T, T>): T;
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

function TList<T>.AggregateOrDefault(const AAggregator: TFunc<T, T, T>; const ADefault: T): T;
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

function TList<T>.All(const APredicate: TFunc<T, Boolean>): Boolean;
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

function TList<T>.Any(const APredicate: TFunc<T, Boolean>): Boolean;
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

procedure TList<T>.Add(const AValue: T);
begin
  { Call Insert }
  Insert(FLength, AValue);
end;

procedure TList<T>.Clear;
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

function TList<T>.Contains(const AValue: T): Boolean;
begin
  { Pass the call to index of }
  Result := (IndexOf(AValue) > -1);
end;

procedure TList<T>.CopyTo(var AArray: array of T);
begin
  { Call the more generic copy to }
  CopyTo(AArray, 0);
end;

function TList<T>.Copy(const StartIndex: Cardinal): TList<T>;
begin
  { Pass the call down to the more generic function }
  Copy(StartIndex, (FLength - StartIndex));
end;

function TList<T>.Copy(const StartIndex, Count: Cardinal): TList<T>;
var
  NewList: TList<T>;

begin
  { Check for zero elements }
  if (FLength = 0) then
  begin
    Result := TList<T>.Create(ElementType);
    Exit;
  end;
  
  { Check for indexes }
  if (StartIndex >= FLength) then
     ExceptionHelper.Throw_ArgumentOutOfRangeError('StartIndex');

  { Check for indexes }
  if ((StartIndex + Count) > FLength) then
     ExceptionHelper.Throw_ArgumentOutOfRangeError('Count');

  { Create a new list }
  NewList := TList<T>.Create(ElementType, Count);

  { Copy all elements safely }
  &Array<T>.SafeMove(FArray, NewList.FArray, StartIndex, 0, Count, ElementType);

  { Set new count }
  NewList.FLength := Count;

  Result := NewList;
end;

procedure TList<T>.CopyTo(var AArray: array of T; const StartIndex: Cardinal);
begin
  { Check for indexes }
  if StartIndex >= Cardinal(Length(AArray)) then
    ExceptionHelper.Throw_ArgumentOutOfRangeError('StartIndex');

  if (Cardinal(Length(AArray)) - StartIndex) < Count then
     ExceptionHelper.Throw_ArgumentOutOfSpaceError('AArray');

  { Copy all elements safely }
  &Array<T>.SafeMove(FArray, AArray, 0, StartIndex, FLength, ElementType);
end;

constructor TList<T>.Create(const AArray: TFixedArray<T>);
begin
  Create(TType<T>.Default, AArray);
end;

constructor TList<T>.Create(const AArray: TDynamicArray<T>);
begin
  Create(TType<T>.Default, AArray);
end;

constructor TList<T>.Create(const AType: IType<T>; const AArray: TFixedArray<T>);
var
  I: Cardinal;
begin
  { Call upper constructor }
  Create(AType);

  { Copy all items in }
  if AArray.Length > 0 then
    for I := 0 to AArray.Length - 1 do
    begin
      Add(AArray[I]);
    end;
end;

constructor TList<T>.Create(const AType: IType<T>; const AArray: TDynamicArray<T>);
var
  I: Cardinal;
begin
  { Call upper constructor }
  Create(AType);

  { Copy all items in }
  if AArray.Length > 0 then
    for I := 0 to AArray.Length - 1 do
    begin
      Add(AArray[I]);
    end;
end;

constructor TList<T>.Create(const AType: IType<T>);
begin
  { Call upper constructor }
  Create(AType, DefaultArrayLength);
end;

constructor TList<T>.Create(const AType: IType<T>;
  const AEnumerable: IEnumerable<T>);
var
  V: T;
begin
  { Call upper constructor }
  Create(AType, DefaultArrayLength);

  { Initialize instance }
  if (AEnumerable = nil) then
     ExceptionHelper.Throw_ArgumentNilError('AEnumerable');

  Add(AEnumerable);
end;

constructor TList<T>.Create;
begin
  Create(TType<T>.Default);
end;

constructor TList<T>.Create(const InitialCapacity: Cardinal);
begin
  Create(TType<T>.Default, InitialCapacity);
end;

constructor TList<T>.Create(const AEnumerable: IEnumerable<T>);
begin
  Create(TType<T>.Default, AEnumerable);
end;

constructor TList<T>.Create(const AType: IType<T>;
  const InitialCapacity: Cardinal);
begin
  { Initialize instance }
  if (AType = nil) then
     ExceptionHelper.Throw_ArgumentNilError('AType');

  { Install the type }
  InstallType(AType);

  FLength := 0;
  FVer := 0;
  SetLength(FArray, InitialCapacity);

  { Call the default (Thread Safety) constructor }
  inherited Create();
end;

procedure TList<T>.DeserializeElement(const AElement: T);
begin
  { Simple as hell ... }
  Add(AElement);
end;

destructor TList<T>.Destroy;
begin
  { Clear list first }
  Clear();

  inherited;
end;

function TList<T>.ElementAt(const Index: Cardinal): T;
begin
  { Simply use the getter }
  Result := GetItem(Index);
end;

function TList<T>.ElementAtOrDefault(const Index: Cardinal; const ADefault: T): T;
begin
  { Check range }
  if (Index >= FLength) then
     Result := ADefault
  else
    Result := FArray[Index];
end;

function TList<T>.Empty: Boolean;
begin
  Result := (FLength = 0);
end;

function TList<T>.EqualsTo(const AEnumerable: IEnumerable<T>): Boolean;
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

function TList<T>.First: T;
begin
  { Check length }
  if FLength = 0 then
    ExceptionHelper.Throw_CollectionEmptyError();

  Result := FArray[0];
end;

function TList<T>.FirstOrDefault(const ADefault: T): T;
begin
  { Check length }
  if FLength = 0 then
    Result := ADefault
  else
    Result := FArray[0];
end;

function TList<T>.GetCapacity: Cardinal;
begin
  Result := Length(FArray);
end;

function TList<T>.GetCount: Cardinal;
begin
  Result := FLength;
end;

function TList<T>.GetEnumerator: IEnumerator<T>;
begin
  { Create an enumerator }
  Result := TEnumerator.Create(Self);
end;

function TList<T>.GetItem(const Index: Cardinal): T;
begin
  { Check range }
  if (Index >= FLength) then
     ExceptionHelper.Throw_ArgumentOutOfRangeError('Index');

  { Get value }
  Result := FArray[Index];
end;

procedure TList<T>.Grow;
begin
  { Grow the array }
  if FLength < DefaultArrayLength then
     SetLength(FArray, FLength + DefaultArrayLength)
  else
     SetLength(FArray, FLength * 2);
end;

function TList<T>.IndexOf(const AValue: T): Integer;
begin
  { Call more generic function }
  Result := IndexOf(AValue, 0, FLength);
end;

function TList<T>.IndexOf(const AValue: T;
  const StartIndex: Cardinal): Integer;
begin
  { Call more generic function }
  Result := IndexOf(AValue, StartIndex, (FLength - StartIndex));
end;

function TList<T>.IndexOf(const AValue: T; const StartIndex,
  Count: Cardinal): Integer;
var
  I: Integer;
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
  for I := StartIndex to ((StartIndex + Count) - 1) do
      if ElementType.AreEqual(FArray[I], AValue) then
      begin
        Result := I;
        Exit;
      end;       
end;

procedure TList<T>.Insert(const AtIndex: Cardinal; const AValue: T);
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

procedure TList<T>.Insert(const AtIndex: Cardinal;
  const AEnumerable: IEnumerable<T>);
var
  V: T;
  I: Integer;
begin
  if AtIndex > FLength then
     ExceptionHelper.Throw_ArgumentOutOfRangeError('AtIndex');

  if (AEnumerable = nil) then
     ExceptionHelper.Throw_ArgumentNilError('AEnumerable');

  I := AtIndex;
  
  { Enumerate and add }
  for V in AEnumerable do
  begin
    Insert(I, V);
    Inc(I);
  end;
end;

function TList<T>.LastIndexOf(const AValue: T;
  const StartIndex: Cardinal): Integer;
begin
  { Call more generic function }
  Result := LastIndexOf(AValue, StartIndex, (FLength - StartIndex));
end;

function TList<T>.Last: T;
begin
  { Check length }
  if FLength = 0 then
    ExceptionHelper.Throw_CollectionEmptyError();

  Result := FArray[FLength - 1];
end;

function TList<T>.LastIndexOf(const AValue: T): Integer;
begin
  { Call more generic function }
  Result := LastIndexOf(AValue, 0, FLength);
end;

function TList<T>.LastOrDefault(const ADefault: T): T;
begin
  { Check length }
  if FLength = 0 then
    Result := ADefault
  else
    Result := FArray[FLength - 1];
end;

function TList<T>.Max: T;
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

function TList<T>.Min: T;
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

function TList<T>.LastIndexOf(const AValue: T; const StartIndex,
  Count: Cardinal): Integer;
var
  I: Integer;
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
  for I := ((StartIndex + Count) - 1) downto StartIndex do
      if ElementType.AreEqual(FArray[I], AValue) then
      begin
        Result := I;
        Exit;
      end;       
end;

procedure TList<T>.Remove(const AValue: T);
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

procedure TList<T>.RemoveAt(const AtIndex: Cardinal);
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

procedure TList<T>.Reverse(const StartIndex, Count: Cardinal);
begin
  { Check for indexes }
  if ((StartIndex + Count) > FLength) then
     ExceptionHelper.Throw_ArgumentOutOfRangeError('StartIndex/Count');

  &Array<T>.Reverse(FArray, StartIndex, Count);
end;

procedure TList<T>.Reverse(const StartIndex: Cardinal);
begin
  { Check for indexes }
  if (StartIndex > FLength) then
     ExceptionHelper.Throw_ArgumentOutOfRangeError('StartIndex');

  &Array<T>.Reverse(FArray, StartIndex, (FLength - StartIndex));
end;

procedure TList<T>.Reverse;
begin
  &Array<T>.Reverse(FArray, 0, FLength);
end;

procedure TList<T>.Sort(const StartIndex, Count: Cardinal; const Ascending: Boolean);
begin
  { Check for indexes }
  if ((StartIndex + Count) > FLength) then
     ExceptionHelper.Throw_ArgumentOutOfRangeError('StartIndex/Count');

  &Array<T>.Sort(FArray, StartIndex, Count, ElementType, Ascending);
end;

procedure TList<T>.Sort(const StartIndex: Cardinal; const Ascending: Boolean);
begin
  { Check for indexes }
  if (StartIndex > FLength) then
     ExceptionHelper.Throw_ArgumentOutOfRangeError('StartIndex');

  &Array<T>.Sort(FArray, StartIndex, (FLength - StartIndex), ElementType, Ascending);
end;

procedure TList<T>.SetItem(const Index: Cardinal; const Value: T);
begin
  { Check range }
  if (Index >= FLength) then
     ExceptionHelper.Throw_ArgumentOutOfRangeError('Index');

  { Get value }
  FArray[Index] := Value;
end;

procedure TList<T>.Shrink;
begin
  { Cut the capacity if required }
  if FLength < Capacity then
  begin
    SetLength(FArray, FLength);
  end;
end;

function TList<T>.Single: T;
begin
  { Check length }
  if FLength = 0 then
    ExceptionHelper.Throw_CollectionEmptyError()
  else if FLength > 1 then
    ExceptionHelper.Throw_CollectionHasMoreThanOneElement()
  else
    Result := FArray[0];
end;

function TList<T>.SingleOrDefault(const ADefault: T): T;
begin
  { Check length }
  if FLength = 0 then
    Result := ADefault
  else if FLength > 1 then
    ExceptionHelper.Throw_CollectionHasMoreThanOneElement()
  else
    Result := FArray[0];
end;

procedure TList<T>.Sort(const Ascending: Boolean);
begin
  &Array<T>.Sort(FArray, 0, FLength, ElementType, Ascending);
end;

procedure TList<T>.StartDeserializing(const AData: TDeserializationData);
begin
  // Do nothing, just say that I am here and I can be serialized
end;

procedure TList<T>.StartSerializing(const AData: TSerializationData);
begin
  // Do nothing, just say that I am here and I can be serialized
end;

function TList<T>.Copy: TList<T>;
begin
  { Call a more generic function }
  Result := Copy(0, FLength);
end;

constructor TList<T>.Create(const AArray: array of T);
begin
  Create(TType<T>.Default, AArray);
end;

constructor TList<T>.Create(const AType: IType<T>; const AArray: array of T);
var
  I: Integer;
begin
  { Call upper constructor }
  Create(AType, DefaultArrayLength);

  { Copy from array }
  for I := 0 to Length(AArray) - 1 do
  begin
    Add(AArray[I]);
  end;
end;

{ TList<T>.TEnumerator }

constructor TList<T>.TEnumerator.Create(const AList: TList<T>);
begin
  { Initialize }
  FList := AList;
  KeepObjectAlive(FList);

  FCurrentIndex := 0;
  FVer := FList.FVer;
end;

destructor TList<T>.TEnumerator.Destroy;
begin
  ReleaseObject(FList);
  inherited;
end;

function TList<T>.TEnumerator.GetCurrent: T;
begin
  if FVer <> FList.FVer then
     ExceptionHelper.Throw_CollectionChangedError();

  if FCurrentIndex > 0 then
    Result := FList.FArray[FCurrentIndex - 1]
  else
    Result := default(T);
end;

function TList<T>.TEnumerator.MoveNext: Boolean;
begin
  if FVer <> FList.FVer then
     ExceptionHelper.Throw_CollectionChangedError();

  Result := FCurrentIndex < FList.FLength;
  Inc(FCurrentIndex);
end;

{ TObjectList<T> }

procedure TObjectList<T>.InstallType(const AType: IType<T>);
begin
  { Create a wrapper over the real type class and switch it }
  FWrapperType := TObjectWrapperType<T>.Create(AType);

  { Install overridden type }
  inherited InstallType(FWrapperType);
end;

function TObjectList<T>.GetOwnsObjects: Boolean;
begin
  Result := FWrapperType.AllowCleanup;
end;

procedure TObjectList<T>.SetOwnsObjects(const Value: Boolean);
begin
  FWrapperType.AllowCleanup := Value;
end;

end.
