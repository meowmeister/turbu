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
unit DeHL.Collections.Heap;
interface
uses SysUtils,
     DeHL.Base,
     DeHL.Types,
     DeHL.Exceptions,
     DeHL.Arrays,
     DeHL.Collections.Base;

type
  { Generic heap - a place to put stuff }
  THeap<T> = class(TEnexCollection<T>, IDynamic)
  private type
    { Internal type to store heap entries }
    TEntry = record
      FNext: Integer;
      FValue: T;
    end;

    { Array of internal entries }
    TEntryArray = TArray<TEntry>;

    { Heap Enumerator }
    TEnumerator = class(TEnumerator<T>)
    private
      FVer         : Cardinal;
      FHeap        : THeap<T>;
      FCurrentIndex: Cardinal;
      FCurrent     : T;
    public
      { Constructor }
      constructor Create(const AHeap: THeap<T>);

      { Destructor }
      destructor Destroy(); override;

      function GetCurrent(): T; override;
      function MoveNext(): Boolean; override;
    end;

  private var
    FArray: TEntryArray;
    FFirstFree: Integer;

    FCount: Cardinal;
    FVer: Cardinal;

    { Setters and getters }
    function GetItem(const AId: Cardinal): T;
    procedure SetItem(const AId: Cardinal; const Value: T);

  protected
    { ICollection support/hidden }
    function GetCount(): Cardinal; override;

    { Gets the current capacity of the collection }
    function GetCapacity(): Cardinal;
  public
    { Constructors }
    constructor Create(); overload;
    constructor Create(const InitialCapacity: Cardinal); overload;
    constructor Create(const AType: IType<T>); overload;
    constructor Create(const AType: IType<T>; const InitialCapacity: Cardinal); overload;

    { Destructor }
    destructor Destroy(); override;

    {  Modification }
    procedure Clear();

    function Add(const AItem: T): Cardinal;
    function Extract(const AId: Cardinal): T;
    procedure Remove(const AId: Cardinal);

    { Lookup }
    function Contains(const AId: Cardinal): Boolean;
    function TryGetValue(const AId: Cardinal; out AValue: T): Boolean;

    { Others }
    property Count: Cardinal read FCount;
    property Capacity: Cardinal read GetCapacity;
    property Items[const AId: Cardinal]: T read GetItem write SetItem; default;

    { Grow/Shrink }
    procedure Shrink();
    procedure Grow();

    { IEnumerable/ ICollection support }
    function GetEnumerator(): IEnumerator<T>; override;

    { Copy-To }
    procedure CopyTo(var AArray: array of T); overload; override;
    procedure CopyTo(var AArray: array of T; const StartIndex: Cardinal); overload; override;

    { Other Enex overrides }
    function Empty(): Boolean; override;
  end;

  { The object variant }
  TObjectHeap<T: class> = class sealed(THeap<T>)
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

{ THeap<T> }

function THeap<T>.Add(const AItem: T): Cardinal;
begin
  { Grow if required }
  if FCount = Cardinal(Length(FArray)) then
    Grow();

  { Adjust the free list }
  Result := FFirstFree;
  FFirstFree := FArray[FFirstFree].FNext;

  { Actually store the value }
  FArray[Result].FNext := -1;
  FArray[Result].FValue := AItem;

  Inc(FVer);
  Inc(FCount);
end;

procedure THeap<T>.Clear;
var
  CV: Boolean;
  I: Integer;
begin
  CV := (ElementType <> nil) and (ElementType.Management = tmManual);

  for I := 0 to Length(FArray) - 1 do
  begin
    if CV and (FArray[I].FNext = -1) then
      ElementType.Cleanup(FArray[I].FValue);

    { Adjust the next free list indices }
    FArray[I].FNext := I + 1;
  end;

  { The first free one starts at zero }
  FFirstFree := 0;

  Inc(FVer);
  FCount := 0;
end;

procedure THeap<T>.CopyTo(var AArray: array of T);
begin
  CopyTo(AArray, 0);
end;

function THeap<T>.Contains(const AId: Cardinal): Boolean;
begin
  { Check the ID }
  Result := (AId < Cardinal(Length(FArray))) and (FArray[AId].FNext = -1);
end;

procedure THeap<T>.CopyTo(var AArray: array of T; const StartIndex: Cardinal);
var
  I, X: Cardinal;
begin
  { Check for indexes }
  if StartIndex >= Cardinal(Length(AArray)) then
    ExceptionHelper.Throw_ArgumentOutOfRangeError('StartIndex');

  if (Cardinal(Length(AArray)) - StartIndex) < FCount then
     ExceptionHelper.Throw_ArgumentOutOfSpaceError('AArray');

  { Copy all good values to the array }
  X := StartIndex;

  { Iterate over the internal array and add what is good }
  for I := 0 to Length(FArray) - 1 do
    if FArray[I].FNext = -1 then
    begin
      AArray[X] := FArray[I].FValue;
      Inc(X);
    end;
end;

constructor THeap<T>.Create;
begin
  Create(TType<T>.Default, DefaultArrayLength);
end;

constructor THeap<T>.Create(const InitialCapacity: Cardinal);
begin
  Create(TType<T>.Default, InitialCapacity);
end;

constructor THeap<T>.Create(const AType: IType<T>);
begin
  Create(AType, DefaultArrayLength);
end;

constructor THeap<T>.Create(const AType: IType<T>; const InitialCapacity: Cardinal);
var
  I: Cardinal;
begin
  { Initialize instance }
  if (AType = nil) then
     ExceptionHelper.Throw_ArgumentNilError('AType');

  InstallType(AType);

  FCount := 0;
  FVer := 0;

  SetLength(FArray, InitialCapacity);

  { Add all new entries to the free list }
  for I := 0 to InitialCapacity - 1 do
    FArray[I].FNext := I + 1;

  FFirstFree := 0;
end;

destructor THeap<T>.Destroy;
begin
  { First, clear myself }
  Clear();

  inherited;
end;

function THeap<T>.Empty: Boolean;
begin
  { Ha! }
  Result := (FCount = 0);
end;

function THeap<T>.Extract(const AId: Cardinal): T;
begin
  { Check the ID }
  if (AId >= Cardinal(Length(FArray))) or (FArray[AId].FNext <> -1) then
    ExceptionHelper.Throw_KeyNotFoundError('AId');

  { Extract the result }
  Result := FArray[AId].FValue;

  { Free this spot for other to use }
  FArray[AId].FNext := FFirstFree;
  FArray[AId].FValue := default(T);
  FFirstFree := AId;

  Inc(FVer);
  Dec(FCount);
end;

function THeap<T>.GetCapacity: Cardinal;
begin
  Result := Length(FArray);
end;

function THeap<T>.GetCount: Cardinal;
begin
  Result := FCount;
end;

function THeap<T>.GetEnumerator: IEnumerator<T>;
begin
  Result := TEnumerator.Create(Self);
end;

function THeap<T>.GetItem(const AId: Cardinal): T;
begin
  { Check the ID }
  if (AId >= Cardinal(Length(FArray))) or (FArray[AId].FNext <> -1) then
    ExceptionHelper.Throw_KeyNotFoundError('AId');

  { Extract the result }
  Result := FArray[AId].FValue;
end;

procedure THeap<T>.Grow;
var
  LNewLength, LOldLength: Cardinal;
  I: Integer;
begin
  LOldLength := Capacity;

  { Calculate the new size }
  if LOldLength < DefaultArrayLength then
     LNewLength := DefaultArrayLength
  else
     LNewLength := LOldLength * 2;

  { Set the new size }
  SetLength(FArray, LNewLength);

  { Add all new entries to the free list }
  for I := LOldLength to LNewLength - 2 do
    FArray[I].FNext := I + 1;

  { Connect the old free list with the newly added one }
  FArray[LNewLength - 1].FNext := FFirstFree;
  FFirstFree := LOldLength;
end;

procedure THeap<T>.Remove(const AId: Cardinal);
var
  LValue: T;
begin
  { Obtain the value at position }
  LValue := Extract(AId);

  { Cleanup the value if necessary }
  if ElementType.Management = tmManual then
    ElementType.Cleanup(LValue);
end;

procedure THeap<T>.SetItem(const AId: Cardinal; const Value: T);
begin
  { Check the ID }
  if (AId >= Cardinal(Length(FArray))) or (FArray[AId].FNext <> -1) then
    ExceptionHelper.Throw_KeyNotFoundError('AId');

  { Cleanup the old inhabitant }
  if ElementType.Management = tmManual then
    ElementType.Cleanup(FArray[AId].FValue);

  { And set the new one }
  FArray[AId].FValue := Value;
end;

procedure THeap<T>.Shrink;
var
  LLen: Integer;
begin
  { Find the last occupied spot }
  LLen := Length(FArray);
  while (LLen > 0) and (FArray[LLen - 1].FNext <> -1) do Dec(LLen);

  { Readjust the array length }
  SetLength(FArray, LLen);
end;

function THeap<T>.TryGetValue(const AId: Cardinal; out AValue: T): Boolean;
begin
  { Check the ID }
  if (AId >= Cardinal(Length(FArray))) or (FArray[AId].FNext <> -1) then
    Exit(false);

  { Extract the result }
  AValue := FArray[AId].FValue;
  Result := true;
end;

{ THeap<T>.TEnumerator }

constructor THeap<T>.TEnumerator.Create(const AHeap: THeap<T>);
begin
  FHeap := AHeap;
  FVer := AHeap.FVer;
  FCurrent := default(T);
  FCurrentIndex := 0;

  KeepObjectAlive(FHeap);
end;

destructor THeap<T>.TEnumerator.Destroy;
begin
  ReleaseObject(FHeap);
  inherited;
end;

function THeap<T>.TEnumerator.GetCurrent: T;
begin
  Result := FCurrent;
end;

function THeap<T>.TEnumerator.MoveNext: Boolean;
begin
  if FVer <> FHeap.FVer then
     ExceptionHelper.Throw_CollectionChangedError();

  { Go over all array and gather what we need }
  while FCurrentIndex < Cardinal(Length(FHeap.FArray)) do
  begin
    { If the spot is occupied, take the value and stop }
    if FHeap.FArray[FCurrentIndex].FNext = -1 then
    begin
      FCurrent := FHeap.FArray[FCurrentIndex].FValue;

      Inc(FCurrentIndex);
      Exit(true);
    end else
      Inc(FCurrentIndex);
  end;

  { All array was walked, nothing found, too bad }
  Result := false;
end;

{ TObjectHeap<T> }

function TObjectHeap<T>.GetOwnsObjects: Boolean;
begin
  Result := FWrapperType.AllowCleanup;
end;

procedure TObjectHeap<T>.InstallType(const AType: IType<T>);
begin
  { Create a wrapper over the real type class and switch it }
  FWrapperType := TObjectWrapperType<T>.Create(AType);

  { Install overridden type }
  inherited InstallType(FWrapperType);
end;

procedure TObjectHeap<T>.SetOwnsObjects(const Value: Boolean);
begin
  FWrapperType.AllowCleanup := Value;
end;

end.
