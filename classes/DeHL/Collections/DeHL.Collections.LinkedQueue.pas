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
unit DeHL.Collections.LinkedQueue;
interface
uses SysUtils,
     DeHL.Base,
     DeHL.Types,
     DeHL.Exceptions,
     DeHL.Serialization,
     DeHL.Arrays,
     DeHL.Collections.LinkedList,
     DeHL.Collections.Base;

type
  { Generic Queue }
  TLinkedQueue<T> = class(TEnexCollection<T>, IQueue<T>)
  private
    FList     : TLinkedList<T>;

  protected
    { Serialization overrides }
    procedure StartSerializing(const AData: TSerializationData); override;
    procedure StartDeserializing(const AData: TDeserializationData); override;
    procedure DeserializeElement(const AElement: T); override;

    { ICollection support/hidden }
    function GetCount(): Cardinal; override;

  public
    { Constructors }
    constructor Create(); overload;
    constructor Create(const AEnumerable: IEnumerable<T>); overload;
    constructor Create(const AArray: array of T); overload;
    constructor Create(const AArray: TDynamicArray<T>); overload;
    constructor Create(const AArray: TFixedArray<T>); overload;

    constructor Create(const AType: IType<T>); overload;
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
    property Count: Cardinal read GetCount;

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
  TObjectLinkedQueue<T: class> = class sealed(TLinkedQueue<T>)
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

{ TLinkedQueue<T> }

function TLinkedQueue<T>.Aggregate(const AAggregator: TFunc<T, T, T>): T;
begin
  { Call the one from the list }
  Result := FList.Aggregate(AAggregator);
end;

function TLinkedQueue<T>.AggregateOrDefault(const AAggregator: TFunc<T, T, T>; const ADefault: T): T;
begin
  { Call the one from the list }
  Result := FList.AggregateOrDefault(AAggregator, ADefault);
end;

function TLinkedQueue<T>.All(const APredicate: TFunc<T, Boolean>): Boolean;
begin
  { Call the one from the list }
  Result := FList.All(APredicate);
end;

function TLinkedQueue<T>.Any(const APredicate: TFunc<T, Boolean>): Boolean;
begin
  { Call the one from the list }
  Result := FList.Any(APredicate);
end;

procedure TLinkedQueue<T>.Clear;
begin
  { Clear the internal list }
  if FList <> nil then
    FList.Clear();
end;

function TLinkedQueue<T>.Contains(const AValue: T): Boolean;
begin
  { Use the list }
  Result := FList.Contains(AValue);
end;

procedure TLinkedQueue<T>.CopyTo(var AArray: array of T; const StartIndex: Cardinal);
begin
  { Invoke the copy-to from the list below }
  FList.CopyTo(AArray, StartIndex);
end;

procedure TLinkedQueue<T>.CopyTo(var AArray: array of T);
begin
  { Forward }
  CopyTo(AArray, 0);
end;

constructor TLinkedQueue<T>.Create(const AType: IType<T>; const AEnumerable: IEnumerable<T>);
var
  V: T;
begin
  { Call upper constructor }
  Create(AType);

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

constructor TLinkedQueue<T>.Create;
begin
  Create(TType<T>.Default);
end;

constructor TLinkedQueue<T>.Create(const AEnumerable: IEnumerable<T>);
begin
  Create(TType<T>.Default, AEnumerable);
end;

constructor TLinkedQueue<T>.Create(const AType: IType<T>);
begin
  { Initialize instance }
  if (AType = nil) then
     ExceptionHelper.Throw_ArgumentNilError('AType');

  { Initialize internals }
  InstallType(AType);

  FList := TLinkedList<T>.Create(ElementType);
end;

function TLinkedQueue<T>.ElementAt(const Index: Cardinal): T;
begin
  { Call the one from the list }
  Result := FList.ElementAt(Index);
end;

function TLinkedQueue<T>.ElementAtOrDefault(const Index: Cardinal; const ADefault: T): T;
begin
  { Call the one from the list }
  Result := FList.ElementAtOrDefault(Index, ADefault);
end;

function TLinkedQueue<T>.Empty: Boolean;
begin
  { Call the one from the list }
  Result := FList.Empty;
end;

procedure TLinkedQueue<T>.Enqueue(const AValue: T);
begin
  { Add a new node to the linked list }
  FList.AddLast(AValue);
end;

function TLinkedQueue<T>.EqualsTo(const AEnumerable: IEnumerable<T>): Boolean;
begin
  { Call the one from the list }
  Result := FList.EqualsTo(AEnumerable);
end;

function TLinkedQueue<T>.First: T;
begin
  { Call the one from the list }
  Result := FList.First;
end;

function TLinkedQueue<T>.FirstOrDefault(const ADefault: T): T;
begin
  { Call the one from the list }
  Result := FList.FirstOrDefault(ADefault);
end;

procedure TLinkedQueue<T>.DeserializeElement(const AElement: T);
begin
  { Simple as hell ... }
  Enqueue(AElement);
end;

destructor TLinkedQueue<T>.Destroy;
begin
  { Cleanup }
  Clear();

  inherited;
end;

function TLinkedQueue<T>.Dequeue: T;
begin
  { Call the list ... again! }
  Result := FList.RemoveAndReturnFirst();
end;

function TLinkedQueue<T>.GetCount: Cardinal;
begin
  Result := FList.Count;
end;

function TLinkedQueue<T>.GetEnumerator: IEnumerator<T>;
begin
  { Get the list enumerator }
  Result := FList.GetEnumerator();
end;

function TLinkedQueue<T>.Last: T;
begin
  { Call the one from the list }
  Result := FList.Last;
end;

function TLinkedQueue<T>.LastOrDefault(const ADefault: T): T;
begin
  { Call the one from the list }
  Result := FList.LastOrDefault(ADefault);
end;

function TLinkedQueue<T>.Max: T;
begin
  { Call the one from the list }
  Result := FList.Max;
end;

function TLinkedQueue<T>.Min: T;
begin
  { Call the one from the list }
  Result := FList.Min;
end;

function TLinkedQueue<T>.Peek: T;
begin
  if FList.FirstNode = nil then
    ExceptionHelper.Throw_CollectionEmptyError();

  Result := FList.FirstNode.Value;
end;

function TLinkedQueue<T>.Single: T;
begin
  { Call the one from the list }
  Result := FList.Single;
end;

function TLinkedQueue<T>.SingleOrDefault(const ADefault: T): T;
begin
  { Call the one from the list }
  Result := FList.SingleOrDefault(ADefault);
end;

procedure TLinkedQueue<T>.StartDeserializing(const AData: TDeserializationData);
begin
  // Do nothing, just say that I am here and I can be serialized
end;

procedure TLinkedQueue<T>.StartSerializing(const AData: TSerializationData);
begin
  // Do nothing, just say that I am here and I can be serialized
end;

constructor TLinkedQueue<T>.Create(const AArray: array of T);
begin
  Create(TType<T>.Default, AArray);
end;

constructor TLinkedQueue<T>.Create(const AType: IType<T>; const AArray: array of T);
var
  I: Integer;
begin
  { Call upper constructor }
  Create(AType);

  { Copy array }
  for I := 0 to Length(AArray) - 1 do
  begin
    Enqueue(AArray[I]);
  end;
end;

constructor TLinkedQueue<T>.Create(const AArray: TFixedArray<T>);
begin
  Create(TType<T>.Default, AArray);
end;

constructor TLinkedQueue<T>.Create(const AArray: TDynamicArray<T>);
begin
  Create(TType<T>.Default, AArray);
end;

constructor TLinkedQueue<T>.Create(const AType: IType<T>; const AArray: TFixedArray<T>);
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

constructor TLinkedQueue<T>.Create(const AType: IType<T>; const AArray: TDynamicArray<T>);
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

{ TObjectLinkedQueue<T> }

procedure TObjectLinkedQueue<T>.InstallType(const AType: IType<T>);
begin
  { Create a wrapper over the real type class and switch it }
  FWrapperType := TObjectWrapperType<T>.Create(AType);

  { Install overridden type }
  inherited InstallType(FWrapperType);
end;

function TObjectLinkedQueue<T>.GetOwnsObjects: Boolean;
begin
  Result := FWrapperType.AllowCleanup;
end;

procedure TObjectLinkedQueue<T>.SetOwnsObjects(const Value: Boolean);
begin
  FWrapperType.AllowCleanup := Value;
end;

end.
