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
unit DeHL.Collections.Stack;
interface
uses SysUtils,
     DeHL.Base,
     DeHL.Types,
     DeHL.Exceptions,
     DeHL.Arrays,
     DeHL.Serialization,
     DeHL.Collections.Base;

type
  { Generic Stack }
  TStack<T> = class(TEnexCollection<T>, IStack<T>, IDynamic)
  private
  type
    { Generic Stack List Enumerator }
    TEnumerator = class(TEnumerator<T>)
    private
      FVer         : Cardinal;
      FStack       : TStack<T>;
      FCurrentIndex: Cardinal;

    public
      { Constructor }
      constructor Create(const AStack: TStack<T>);

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

    {  Modification }
    procedure Clear();
    procedure Push(const AValue: T);
    function Pop(): T;
    function Peek(): T;

    procedure Remove(const AValue: T);

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
  TObjectStack<T: class> = class sealed(TStack<T>)
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
  DefaultArrayLength = 10;

{ TStack<T> }

function TStack<T>.Aggregate(const AAggregator: TFunc<T, T, T>): T;
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

function TStack<T>.AggregateOrDefault(const AAggregator: TFunc<T, T, T>; const ADefault: T): T;
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

function TStack<T>.All(const APredicate: TFunc<T, Boolean>): Boolean;
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

function TStack<T>.Any(const APredicate: TFunc<T, Boolean>): Boolean;
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

procedure TStack<T>.Clear;
var
  I: Integer;
begin
  if (ElementType <> nil) and (ElementType.Management() = tmManual) and (FLength > 0) then
  begin
    { Should cleanup each element individually }
    for I := 0 to FLength - 1 do
      ElementType.Cleanup(FArray[I]);
  end;

  { Simply reset all to default }
  SetLength(FArray, DefaultArrayLength);
  FLength := 0;

  Inc(FVer);
end;

function TStack<T>.Contains(const AValue: T): Boolean;
var
  I: Integer;
begin
  { Defaults }
  Result := False;
  if (FLength = 0) then Exit;

  for I := 0 to FLength - 1 do
  begin
    if ElementType.AreEqual(FArray[I], AValue) then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

procedure TStack<T>.CopyTo(var AArray: array of T; const StartIndex: Cardinal);
begin
  { Check for indexes }
  if StartIndex >= Cardinal(Length(AArray)) then
    ExceptionHelper.Throw_ArgumentOutOfRangeError('StartIndex');

  if (Cardinal(Length(AArray)) - StartIndex) < FLength then
     ExceptionHelper.Throw_ArgumentOutOfSpaceError('AArray');

  { Copy all elements safely }
  &Array<T>.SafeMove(FArray, AArray, 0, StartIndex, FLength, ElementType);
end;

procedure TStack<T>.CopyTo(var AArray: array of T);
begin
  { Pass to the generic function }
  CopyTo(AArray, 0);
end;

constructor TStack<T>.Create(const AType: IType<T>; const AEnumerable: IEnumerable<T>);
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
    Push(V);
  end;
end;

constructor TStack<T>.Create;
begin
  Create(TType<T>.Default);
end;

constructor TStack<T>.Create(const InitialCapacity: Cardinal);
begin
  Create(TType<T>.Default, InitialCapacity);
end;

constructor TStack<T>.Create(const AEnumerable: IEnumerable<T>);
begin
  Create(TType<T>.Default, AEnumerable);
end;

constructor TStack<T>.Create(const AType: IType<T>);
begin
  { Call upper constructor }
  Create(AType, DefaultArrayLength);
end;

constructor TStack<T>.Create(const AType: IType<T>; const InitialCapacity: Cardinal);
begin
  { Initialize instance }
  if (AType = nil) then
     ExceptionHelper.Throw_ArgumentNilError('AType');

  InstallType(AType);

  FLength := 0;
  FVer := 0;
  SetLength(FArray, InitialCapacity);

  { Call the default (Thread Safety) constructor }
  inherited Create();
end;

procedure TStack<T>.DeserializeElement(const AElement: T);
begin
  { Simple as hell ... }
  Push(AElement);
end;

destructor TStack<T>.Destroy;
begin
  { Some clean-up }
  Clear();

  inherited;
end;

function TStack<T>.ElementAt(const Index: Cardinal): T;
begin
  if (Index >= FLength) then
    ExceptionHelper.Throw_ArgumentOutOfRangeError('Index');

  Result := FArray[Index];
end;

function TStack<T>.ElementAtOrDefault(const Index: Cardinal; const ADefault: T): T;
begin
  { Check range }
  if (Index >= FLength) then
     Result := ADefault
  else
     Result := FArray[Index];
end;

function TStack<T>.Empty: Boolean;
begin
  Result := (FLength = 0);
end;

function TStack<T>.EqualsTo(const AEnumerable: IEnumerable<T>): Boolean;
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

function TStack<T>.First: T;
begin
  { Check length }
  if FLength = 0 then
    ExceptionHelper.Throw_CollectionEmptyError();

  Result := FArray[0];
end;

function TStack<T>.FirstOrDefault(const ADefault: T): T;
begin
  { Check length }
  if FLength = 0 then
    Result := ADefault
  else
    Result := FArray[0];
end;

function TStack<T>.GetCapacity: Cardinal;
begin
  Result := Length(FArray);
end;

function TStack<T>.GetCount: Cardinal;
begin
  { Use the variable }
  Result := FLength;
end;

function TStack<T>.GetEnumerator: IEnumerator<T>;
begin
  Result := TEnumerator.Create(Self);
end;

procedure TStack<T>.Grow;
var
  ListLength: Cardinal;
begin
  ListLength := Capacity;

  if ListLength = 0 then
     ListLength := DefaultArrayLength
  else
     ListLength := ListLength * 2;

  SetLength(FArray, ListLength);
end;

function TStack<T>.Last: T;
begin
  { Check length }
  if FLength = 0 then
    ExceptionHelper.Throw_CollectionEmptyError();

  Result := FArray[FLength - 1];
end;

function TStack<T>.LastOrDefault(const ADefault: T): T;
begin
  { Check length }
  if FLength = 0 then
    Result := ADefault
  else
    Result := FArray[FLength - 1];
end;

function TStack<T>.Max: T;
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

function TStack<T>.Min: T;
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

function TStack<T>.Peek: T;
begin
  if FLength > 0 then
     Result := FArray[FLength - 1]
  else
     ExceptionHelper.Throw_CollectionEmptyError();
end;

function TStack<T>.Pop: T;
begin
  if FLength > 0 then
  begin
     Result := FArray[FLength - 1];
     Dec(FLength);
     Inc(FVer);
  end
  else
     ExceptionHelper.Throw_CollectionEmptyError();
end;

procedure TStack<T>.Push(const AValue: T);
begin
  { Enure enough capacity }
  if (FLength >= Capacity) then
    Grow();

  { Add the element to the stack and increase the index }
  FArray[FLength] := AValue;
  Inc(FLength);
  Inc(FVer);
end;

procedure TStack<T>.Remove(const AValue: T);
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

  if (FoundIndex > -1) then
  begin
    { Move the list }
    if FLength > 1 then
      for I := FoundIndex to FLength - 2 do
        FArray[I] := FArray[I + 1];

    Dec(FLength);
    Inc(FVer);
  end;
end;

procedure TStack<T>.Shrink;
begin
  { Cut the capacity if required }
  if FLength < Capacity then
  begin
    SetLength(FArray, FLength);
  end;
end;

function TStack<T>.Single: T;
begin
  { Check length }
  if FLength = 0 then
    ExceptionHelper.Throw_CollectionEmptyError()
  else if FLength > 1 then
    ExceptionHelper.Throw_CollectionHasMoreThanOneElement()
  else
    Result := FArray[0];
end;

function TStack<T>.SingleOrDefault(const ADefault: T): T;
begin
  { Check length }
  if FLength = 0 then
    Result := ADefault
  else if FLength > 1 then
    ExceptionHelper.Throw_CollectionHasMoreThanOneElement()
  else
    Result := FArray[0];
end;

procedure TStack<T>.StartDeserializing(const AData: TDeserializationData);
begin
  // Do nothing, just say that I am here and I can be serialized
end;

procedure TStack<T>.StartSerializing(const AData: TSerializationData);
begin
  // Do nothing, just say that I am here and I can be serialized
end;

constructor TStack<T>.Create(const AArray: array of T);
begin
  Create(TType<T>.Default, AArray);
end;

constructor TStack<T>.Create(const AType: IType<T>; const AArray: array of T);
var
  I: Integer;
begin
  { Call upper constructor }
  Create(AType, DefaultArrayLength);

  { Copy array }
  for I := 0 to Length(AArray) - 1 do
  begin
    Push(AArray[I]);
  end;
end;

constructor TStack<T>.Create(const AArray: TFixedArray<T>);
begin
  Create(TType<T>.Default, AArray);
end;

constructor TStack<T>.Create(const AArray: TDynamicArray<T>);
begin
  Create(TType<T>.Default, AArray);
end;

constructor TStack<T>.Create(const AType: IType<T>; const AArray: TFixedArray<T>);
var
  I: Cardinal;
begin
  { Call upper constructor }
  Create(AType);

  { Copy all items in }
  if AArray.Length > 0 then
    for I := 0 to AArray.Length - 1 do
    begin
      Push(AArray[I]);
    end;
end;

constructor TStack<T>.Create(const AType: IType<T>; const AArray: TDynamicArray<T>);
var
  I: Cardinal;
begin
  { Call upper constructor }
  Create(AType);

  { Copy all items in }
  if AArray.Length > 0 then
    for I := 0 to AArray.Length - 1 do
    begin
      Push(AArray[I]);
    end;
end;

{ TStack<T>.TEnumerator }

constructor TStack<T>.TEnumerator.Create(const AStack: TStack<T>);
begin
  { Initialize }
  FStack := AStack;
  KeepObjectAlive(FStack);

  FCurrentIndex := 0;
  FVer := AStack.FVer;
end;

destructor TStack<T>.TEnumerator.Destroy;
begin
  ReleaseObject(FStack);
  inherited;
end;

function TStack<T>.TEnumerator.GetCurrent: T;
begin
  if FVer <> FStack.FVer then
     ExceptionHelper.Throw_CollectionChangedError();

  if FCurrentIndex > 0 then
    Result := FStack.FArray[FCurrentIndex - 1]
  else
    Result := default(T);
end;

function TStack<T>.TEnumerator.MoveNext: Boolean;
begin
  if FVer <> FStack.FVer then
     ExceptionHelper.Throw_CollectionChangedError();

  Result := FCurrentIndex < FStack.FLength;
  Inc(FCurrentIndex);
end;

{ TObjectStack<T> }

procedure TObjectStack<T>.InstallType(const AType: IType<T>);
begin
  { Create a wrapper over the real type class and switch it }
  FWrapperType := TObjectWrapperType<T>.Create(AType);

  { Install overridden type }
  inherited InstallType(FWrapperType);
end;

function TObjectStack<T>.GetOwnsObjects: Boolean;
begin
  Result := FWrapperType.AllowCleanup;
end;

procedure TObjectStack<T>.SetOwnsObjects(const Value: Boolean);
begin
  FWrapperType.AllowCleanup := Value;
end;

end.
