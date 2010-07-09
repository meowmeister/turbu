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
unit DeHL.Collections.Queue;
interface
uses SysUtils,
     DeHL.Base,
     DeHL.Types,
     DeHL.Exceptions,
     DeHL.Arrays,
     DeHL.Serialization,
     DeHL.Collections.Base;

type
  { Generic Queue }
  TQueue<T> = class(TEnexCollection<T>, IQueue<T>, IDynamic)
  private
  type
    { Generic Stack List Enumerator }
    TEnumerator = class(TEnumerator<T>)
    private
      FVer          : Cardinal;
      FQueue        : TQueue<T>;
      FElement      : T;
      FCount, FHead : Cardinal;

    public
      { Constructor }
      constructor Create(const AQueue : TQueue<T>);

      { Destructor }
      destructor Destroy(); override;

      function GetCurrent(): T; override;
      function MoveNext(): Boolean; override;
    end;

  var
    FVer       : Cardinal;
    FHead      : Cardinal;
    FTail      : Cardinal;
    FLength    : Cardinal;
    FArray     : TArray<T>;

    procedure SetCapacity(NewCapacity : Cardinal);
    
  protected
    { Serialization overrides }
    procedure StartSerializing(const AData: TSerializationData); override;
    procedure StartDeserializing(const AData: TDeserializationData); override;
    procedure DeserializeElement(const AElement: T); override;

    { ICollection support/hidden }
    function GetCount() : Cardinal; override;

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

    {  Modification }
    procedure Clear();

    procedure Enqueue(const AValue: T);
    function Dequeue(): T;
    function Peek(): T;

    { Look-up }
    function Contains(const AValue: T): Boolean;

    { Others }
    property Count: Cardinal read FLength;
    property Capacity: Cardinal read GetCapacity;

    { Grow/Shrink }
    procedure Shrink();
    procedure Grow();

    { IEnumerable/ ICollection support }
    function GetEnumerator(): IEnumerator<T>; override;

    { Copy-To }
    procedure CopyTo(var AArray: array of T); overload; override;
    procedure CopyTo(var AArray: array of T; const StartIndex: Cardinal); overload; override;

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
  TObjectQueue<T: class> = class sealed(TQueue<T>)
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
  DefaultArrayLength = 8;

{ TQueue<T> }

function TQueue<T>.Aggregate(const AAggregator: TFunc<T, T, T>): T;
var
  I, H: Cardinal;
begin
  { Check arguments }
  if not Assigned(AAggregator) then
    ExceptionHelper.Throw_ArgumentNilError('AAggregator');

  if FLength = 0 then
    ExceptionHelper.Throw_CollectionEmptyError();

  { Select the first element as comparison base }
  Result := FArray[FHead];

  H := (FHead + 1) mod Cardinal(Length(FArray));

  for I := 1 to FLength - 1 do
  begin
    { Aggregate a value }
    Result := AAggregator(Result, FArray[H]);

    { Circulate Head }
    H := (H + 1) mod Cardinal(Length(FArray));
  end;
end;

function TQueue<T>.AggregateOrDefault(const AAggregator: TFunc<T, T, T>; const ADefault: T): T;
var
  I, H: Cardinal;
begin
  { Check arguments }
  if not Assigned(AAggregator) then
    ExceptionHelper.Throw_ArgumentNilError('AAggregator');

  if FLength = 0 then
    Exit(ADefault);

  { Select the first element as comparison base }
  Result := FArray[FHead];

  H := (FHead + 1) mod Cardinal(Length(FArray));

  for I := 1 to FLength - 1 do
  begin
    { Aggregate a value }
    Result := AAggregator(Result, FArray[H]);

    { Circulate Head }
    H := (H + 1) mod Cardinal(Length(FArray));
  end;
end;

function TQueue<T>.All(const APredicate: TFunc<T, Boolean>): Boolean;
var
  I, H: Cardinal;
begin
  if not Assigned(APredicate) then
    ExceptionHelper.Throw_ArgumentNilError('APredicate');

  if FLength > 0 then
  begin
    H := FHead;
    for I := 0 to FLength - 1 do
    begin
      if not APredicate(FArray[H]) then
        Exit(false);

      { Circulate Head }
      H := (H + 1) mod Cardinal(Length(FArray));
    end;
  end;

  Result := true;
end;

function TQueue<T>.Any(const APredicate: TFunc<T, Boolean>): Boolean;
var
  I, H: Cardinal;
begin
  if not Assigned(APredicate) then
    ExceptionHelper.Throw_ArgumentNilError('APredicate');

  if FLength > 0 then
  begin
    H := FHead;
    for I := 0 to FLength - 1 do
    begin
      if APredicate(FArray[H]) then
        Exit(true);

      { Circulate Head }
      H := (H + 1) mod Cardinal(Length(FArray));
    end;
  end;

  Result := false;
end;

procedure TQueue<T>.Clear;
var
  Element: T;
begin
  if (ElementType <> nil) and (ElementType.Management() = tmManual) then
  begin
    { If must cleanup, use the dequeue method }
    while Count > 0 do
    begin
      Element := Dequeue();
      ElementType.Cleanup(Element);
    end;
  end else
  begin
    { Clear all internals }
    FTail := 0;
    FHead := 0;
    FLength := 0;

    Inc(FVer);
  end;
end;

function TQueue<T>.Contains(const AValue: T): Boolean;
var
  I       : Cardinal;
  Capacity: Cardinal;
begin
  { Do a look-up in all the queue }
  Result := False;

  I := FHead;
  Capacity := Length(FArray);

  while I <> FTail do
  begin
    if ElementType.AreEqual(FArray[I], AValue) then
    begin
      Result := True;
      Break;
    end;

    { Next + wrap over }
    I := (I + 1) mod Capacity;
  end;

end;
                 
procedure TQueue<T>.CopyTo(var AArray: array of T; const StartIndex: Cardinal);
var
  I, X    : Cardinal;
  Capacity: Cardinal;

begin
  { Check for indexes }
  if StartIndex >= Cardinal(Length(AArray)) then
    ExceptionHelper.Throw_ArgumentOutOfRangeError('StartIndex');

  if (Cardinal(Length(AArray)) - StartIndex) < Count then
     ExceptionHelper.Throw_ArgumentOutOfSpaceError('AArray');

  X := StartIndex;
  I := FHead;
  Capacity := Length(FArray);

  while FTail <> I do
  begin
    { Copy value }
    AArray[X] := FArray[I];

    { Next + wrap over }
    I := (I + 1) mod Capacity;
    Inc(X);
  end;
end;

procedure TQueue<T>.CopyTo(var AArray: array of T);
begin
  { Forward }
  CopyTo(AArray, 0);
end;

constructor TQueue<T>.Create(const AType: IType<T>;
  const AEnumerable: IEnumerable<T>);
var
  V: T;
begin
  { Call upper constructor }
  Create(AType, DefaultArrayLength);

  { Initialize instance }
  if (AEnumerable = nil) then
     ExceptionHelper.Throw_ArgumentNilError('AEnumerable');

  { Try to copy the given Enumerable }
  for V in AEnumerable do
  begin
    { Perform a simple push }
    Enqueue(V);
  end;
end;

constructor TQueue<T>.Create;
begin
  Create(TType<T>.Default);
end;

constructor TQueue<T>.Create(const InitialCapacity: Cardinal);
begin
  Create(TType<T>.Default, InitialCapacity);
end;

constructor TQueue<T>.Create(const AEnumerable: IEnumerable<T>);
begin
  Create(TType<T>.Default, AEnumerable);
end;

constructor TQueue<T>.Create(const AType: IType<T>;
  const InitialCapacity: Cardinal);
begin
  { Initialize instance }
  if (AType = nil) then
     ExceptionHelper.Throw_ArgumentNilError('AType');

  InstallType(AType);

  FVer := 0;
  FTail := 0;
  FLength := 0;
  FHead := 0;
  SetLength(FArray, InitialCapacity);

  { Call the default (Thread Safety) constructor }
  inherited Create();
end;

constructor TQueue<T>.Create(const AType: IType<T>);
begin
  { Call upper constructor }
  Create(AType, DefaultArrayLength);
end;

function TQueue<T>.ElementAt(const Index: Cardinal): T;
var
  H: Cardinal;
begin
  if (Index >= FLength) then
    ExceptionHelper.Throw_ArgumentOutOfRangeError('Index');

  H := (FHead + Index) mod Cardinal(Length(FArray));
  Result := FArray[H];
end;

function TQueue<T>.ElementAtOrDefault(const Index: Cardinal; const ADefault: T): T;
var
  H: Cardinal;
begin
  if (Index >= FLength) then
    Exit(ADefault);

  H := (FHead + Index) mod Cardinal(Length(FArray));
  Result := FArray[H];
end;

function TQueue<T>.Empty: Boolean;
begin
  Result := (FLength = 0);
end;

procedure TQueue<T>.Enqueue(const AValue: T);
var
  NewCapacity: Cardinal;
begin
  { Ensure Capacity }
  if FLength = Cardinal(Length(FArray)) then
  begin
    NewCapacity := Length(FArray) * 2;

    if NewCapacity < DefaultArrayLength then
       NewCapacity := Length(FArray) + DefaultArrayLength;

    SetCapacity(NewCapacity);
  end;

  { Place the element to the end of the list }
  FArray[FTail] := AValue;  
  FTail := (FTail + 1) mod Cardinal(Length(FArray));
  
  Inc(FLength);
  Inc(FVer);
end;

function TQueue<T>.EqualsTo(const AEnumerable: IEnumerable<T>): Boolean;
var
  V: T;
  I, H: Cardinal;
begin
  I := 0;
  H := FHead;

  for V in AEnumerable do
  begin
    if I >= FLength then
      Exit(false);

    if not ElementType.AreEqual(FArray[H], V) then
      Exit(false);

    H := (H + 1) mod Cardinal(Length(FArray));
    Inc(I);
  end;

  if I < FLength then
    Exit(false);

  Result := true;
end;

function TQueue<T>.First: T;
begin
  { Check length }
  if FLength = 0 then
    ExceptionHelper.Throw_CollectionEmptyError();

  Result := FArray[FHead];
end;

function TQueue<T>.FirstOrDefault(const ADefault: T): T;
begin
  { Check length }
  if FLength = 0 then
    Result := ADefault
  else
    Result := FArray[FHead];
end;

procedure TQueue<T>.DeserializeElement(const AElement: T);
begin
  { Simple as hell ... }
  Enqueue(AElement);
end;

destructor TQueue<T>.Destroy;
begin
  { Cleanup }
  Clear();

  inherited;
end;

function TQueue<T>.Dequeue: T;
begin
  if FLength = 0 then
    ExceptionHelper.Throw_CollectionEmptyError();

  { Get the head }
  Result := FArray[FHead];

  { Circulate Head }
  FHead := (FHead + 1) mod Cardinal(Length(FArray));

  Dec(FLength);
  Inc(FVer);
end;

function TQueue<T>.GetCapacity: Cardinal;
begin
  Result := Length(FArray);
end;

function TQueue<T>.GetCount: Cardinal;
begin
  Result := FLength;
end;

function TQueue<T>.GetEnumerator: IEnumerator<T>;
begin
  Result := TEnumerator.Create(Self);
end;

procedure TQueue<T>.Grow;
var
  NewCapacity: Cardinal;
begin
  { Ensure Capacity }
  if FLength = Cardinal(Length(FArray)) then
  begin
    NewCapacity := Length(FArray) * 2;

    if NewCapacity < DefaultArrayLength then
       NewCapacity := Length(FArray) + DefaultArrayLength;

    SetCapacity(NewCapacity);
  end;
end;

function TQueue<T>.Last: T;
var
  T: Cardinal;
begin
  { Check length }
  if FLength = 0 then
    ExceptionHelper.Throw_CollectionEmptyError();

  T := (FTail - 1) mod Cardinal(Length(FArray));
  Result := FArray[T];
end;

function TQueue<T>.LastOrDefault(const ADefault: T): T;
var
  T: Cardinal;
begin
  { Check length }
  if FLength = 0 then
    Result := ADefault
  else
  begin
    T := (FTail - 1) mod Cardinal(Length(FArray));
    Result := FArray[T];
  end;
end;

function TQueue<T>.Max: T;
var
  I, H: Cardinal;
begin
  { Check length }
  if FLength = 0 then
    ExceptionHelper.Throw_CollectionEmptyError();

  { Default one }
  H := FHead;
  Result := FArray[H];

  H := (H + 1) mod Cardinal(Length(FArray));

  for I := 1 to FLength - 1 do
  begin
    if ElementType.Compare(FArray[H], Result) > 0 then
      Result := FArray[I];

    { Circulate Head }
    H := (H + 1) mod Cardinal(Length(FArray));
  end;
end;

function TQueue<T>.Min: T;
var
  I, H: Cardinal;
begin
  { Check length }
  if FLength = 0 then
    ExceptionHelper.Throw_CollectionEmptyError();

  { Default one }
  H := FHead;
  Result := FArray[H];

  H := (H + 1) mod Cardinal(Length(FArray));

  for I := 1 to FLength - 1 do
  begin
    if ElementType.Compare(FArray[H], Result) < 0 then
      Result := FArray[I];

    { Circulate Head }
    H := (H + 1) mod Cardinal(Length(FArray));
  end;
end;

function TQueue<T>.Peek: T;
begin
  if FTail = FHead then
    ExceptionHelper.Throw_CollectionEmptyError();

  Result := FArray[FHead];
end;

procedure TQueue<T>.SetCapacity(NewCapacity: Cardinal);
var
 NewArray: TArray<T>;
begin
  { Create new array }
  SetLength(NewArray, NewCapacity);

  if (FLength > 0) then
  begin
    if FHead < FTail then
       Move(FArray[FHead], NewArray[0], FLength * SizeOf(T))
    else
    begin
       Move(FArray[FHead], NewArray[0], (FLength - FHead) * SizeOf(T));
       Move(FArray[0], NewArray[Cardinal(Length(FArray)) - FHead], FTail * SizeOf(T));
    end;
  end;

  { Switch arrays }
  FArray := nil;
  FArray := NewArray;
  
  FTail := FLength;
  FHead := 0;
  Inc(FVer);
end;

procedure TQueue<T>.Shrink;
begin
  { Ensure Capacity }
  if FLength < Capacity then
  begin
    SetCapacity(FLength);
  end;
end;

function TQueue<T>.Single: T;
begin
  { Check length }
  if FLength = 0 then
    ExceptionHelper.Throw_CollectionEmptyError()
  else if FLength > 1 then
    ExceptionHelper.Throw_CollectionHasMoreThanOneElement()
  else
    Result := FArray[FHead];
end;

function TQueue<T>.SingleOrDefault(const ADefault: T): T;
begin
  { Check length }
  if FLength = 0 then
    Result := ADefault
  else if FLength > 1 then
    ExceptionHelper.Throw_CollectionHasMoreThanOneElement()
  else
    Result := FArray[FHead];
end;

procedure TQueue<T>.StartDeserializing(const AData: TDeserializationData);
begin
  // Do nothing, just say that I am here and I can be serialized
end;

procedure TQueue<T>.StartSerializing(const AData: TSerializationData);
begin
  // Do nothing, just say that I am here and I can be serialized
end;

constructor TQueue<T>.Create(const AArray: array of T);
begin
  Create(TType<T>.Default, AArray);
end;

constructor TQueue<T>.Create(const AType: IType<T>; const AArray: array of T);
var
  I: Integer;
begin
  { Call upper constructor }
  Create(AType, DefaultArrayLength);

  { Copy array }
  for I := 0 to Length(AArray) - 1 do
  begin
    Enqueue(AArray[I]);
  end;
end;

constructor TQueue<T>.Create(const AArray: TFixedArray<T>);
begin
  Create(TType<T>.Default, AArray);
end;

constructor TQueue<T>.Create(const AArray: TDynamicArray<T>);
begin
  Create(TType<T>.Default, AArray);
end;

constructor TQueue<T>.Create(const AType: IType<T>; const AArray: TFixedArray<T>);
var
  I: Cardinal;
begin
  { Call upper constructor }
  Create(AType);

  { Copy all items in }
  if AArray.Length > 0 then
    for I := 0 to AArray.Length - 1 do
    begin
      Enqueue(AArray[I]);
    end;
end;

constructor TQueue<T>.Create(const AType: IType<T>; const AArray: TDynamicArray<T>);
var
  I: Cardinal;
begin
  { Call upper constructor }
  Create(AType);

  { Copy all items in }
  if AArray.Length > 0 then
    for I := 0 to AArray.Length - 1 do
    begin
      Enqueue(AArray[I]);
    end;
end;

{ TQueue<T>.TEnumerator }

constructor TQueue<T>.TEnumerator.Create(const AQueue: TQueue<T>);
begin
  { Initialize }
  FQueue := AQueue;
  KeepObjectAlive(FQueue);

  FCount := 0;
  FElement := Default(T);
  FHead  := FQueue.FHead;
  FVer := AQueue.FVer;
end;

destructor TQueue<T>.TEnumerator.Destroy;
begin
  ReleaseObject(FQueue);
  inherited;
end;

function TQueue<T>.TEnumerator.GetCurrent: T;
begin
  if FVer <> FQueue.FVer then
     ExceptionHelper.Throw_CollectionChangedError();

  Result := FElement;
end;

function TQueue<T>.TEnumerator.MoveNext: Boolean;
begin
  if FVer <> FQueue.FVer then
     ExceptionHelper.Throw_CollectionChangedError();

  if (FCount >= FQueue.FLength) then
    Exit(false)
  else
    Result := true;

  FElement := FQueue.FArray[FHead];

  { Circulate Head }
  FHead := (FHead + 1) mod Cardinal(Length(FQueue.FArray));
  Inc(FCount);
end;

{ TObjectQueue<T> }

procedure TObjectQueue<T>.InstallType(const AType: IType<T>);
begin
  { Create a wrapper over the real type class and switch it }
  FWrapperType := TObjectWrapperType<T>.Create(AType);

  { Install overridden type }
  inherited InstallType(FWrapperType);
end;

function TObjectQueue<T>.GetOwnsObjects: Boolean;
begin
  Result := FWrapperType.AllowCleanup;
end;

procedure TObjectQueue<T>.SetOwnsObjects(const Value: Boolean);
begin
  FWrapperType.AllowCleanup := Value;
end;

end.
