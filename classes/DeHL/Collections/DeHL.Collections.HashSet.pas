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
unit DeHL.Collections.HashSet;
interface
uses SysUtils,
     DeHL.Base,
     DeHL.Types,
     DeHL.Serialization,
     DeHL.Exceptions,
     DeHL.Math.Primes,
     DeHL.KeyValuePair,
     DeHL.Collections.Base,
     DeHL.Arrays;

type
  { Generic Dictionary }
  THashSet<T> = class(TEnexCollection<T>, ISet<T>)
  private
  type
    { Generic Dictionary Keys Enumerator }
    TEnumerator = class(TEnumerator<T>)
    private
      FVer          : Cardinal;
      FDict         : THashSet<T>;
      FCurrentIndex : Integer;
      FValue        : T;

    public
      { Constructor }
      constructor Create(const ADict : THashSet<T>);

      { Destructor }
      destructor Destroy(); override;

      function GetCurrent(): T; override;
      function MoveNext(): Boolean; override;
    end;

    TEntry = record
      FHashCode : Integer;
      FNext     : Integer;
      FKey      : T;
    end;

    TBucketArray = array of Integer;

  var
    FBucketArray   : TBucketArray;
    FEntryArray    : TArray<TEntry>;
    FCount         : Integer;
    FFreeCount     : Integer;
    FFreeList      : Integer;
    FVer           : Cardinal;

    { Internal }
    procedure InitializeInternals(const Capacity : Cardinal);
    procedure Insert(const AKey : T; const ShouldAdd : Boolean = true);
    function FindEntry(const AKey : T) : Integer;
    procedure Resize();
    function Hash(const AKey : T) : Integer;

  protected
    { Serialization overrides }
    procedure StartSerializing(const AData: TSerializationData); override;
    procedure StartDeserializing(const AData: TDeserializationData); override;
    procedure DeserializeElement(const AElement: T); override;

    { Hidden }
    function GetCount() : Cardinal; override;

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

    procedure Add(const AValue : T); virtual;
    procedure Remove(const AValue: T); virtual;

    { Lookup }
    function Contains(const AValue: T): Boolean; virtual;

    { Properties }
    property Count : Cardinal read GetCount;

    { IEnumerable/ ICollection support }
    function GetEnumerator() : IEnumerator<T>; override;

    { Copy-To }

    procedure CopyTo(var AArray : array of T); overload; override;
    procedure CopyTo(var AArray : array of T; const StartIndex : Cardinal); overload; override;

    { Enex Overrides }
    function Empty(): Boolean; override;
  end;

  { The object variant }
  TObjectHashSet<T: class> = class sealed(THashSet<T>)
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

{ THashSet<T> }

procedure THashSet<T>.Add(const AValue: T);
begin
 { Call insert }
 Insert(AValue, False);
end;

procedure THashSet<T>.Clear;
var
  I : Integer;
  KC, MKC: Boolean;
begin
  if FCount > 0 then
  begin
    for I := 0 to Length(FBucketArray) - 1 do
        FBucketArray[I] := -1;
  end;

  { Cleanup each key if necessary }
  if (Length(FEntryArray) > 0) then
  begin
    KC := (ElementType <> nil) and (ElementType.Management = tmManual);
    MKC := (ElementType <> nil) and (ElementType.Management = tmCompiler);

    if KC or MKC then
    begin
      for I := 0 to Length(FEntryArray) - 1 do
        if FEntryArray[I].FHashCode >= 0 then
        begin
          if KC then
            ElementType.Cleanup(FEntryArray[I].FKey)

          else if MKC then
            FEntryArray[I].FKey := default(T);
        end;
    end;
  end;

  if Length(FEntryArray) > 0 then
     FillChar(FEntryArray[0], Length(FEntryArray) * SizeOf(TEntry), 0);

  FFreeList := -1;
  FCount := 0;
  FFreeCount := 0;

  Inc(FVer);
end;

function THashSet<T>.Contains(const AValue: T): Boolean;
begin
  Result := (FindEntry(AValue) >= 0);
end;

procedure THashSet<T>.CopyTo(var AArray: array of T);
begin
  { Call the more generic function }
  CopyTo(AArray, 0);
end;

procedure THashSet<T>.CopyTo(
  var AArray: array of T; const StartIndex: Cardinal);
var
  I, X : Integer;
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
       AArray[X] := FEntryArray[I].FKey;
       Inc(X);
    end;
  end;
end;

constructor THashSet<T>.Create;
begin
  Create(TType<T>.Default);
end;

constructor THashSet<T>.Create(const InitialCapacity: Cardinal);
begin
  Create(TType<T>.Default, InitialCapacity);
end;

constructor THashSet<T>.Create(const AEnumerable: IEnumerable<T>);
begin
  Create(TType<T>.Default, AEnumerable);
end;

constructor THashSet<T>.Create(const AType: IType<T>; const InitialCapacity: Cardinal);
begin
  { Initialize instance }
  if (AType = nil) then
     ExceptionHelper.Throw_ArgumentNilError('AType');

  { Install the type }
  InstallType(AType);

  FVer := 0;
  FCount := 0;
  FFreeCount := 0;
  FFreeList := 0;

  InitializeInternals(DefaultArrayLength);

  { Call the default (Thread Safety) constructor }
  inherited Create();
end;

constructor THashSet<T>.Create(const AType: IType<T>; const AEnumerable: IEnumerable<T>);
var
  V : T;
begin
  { Call upper constructor }
  Create(AType, DefaultArrayLength);

  if (AEnumerable = nil) then
     ExceptionHelper.Throw_ArgumentNilError('AEnumerable');

  { Pump in all items }
  for V in AEnumerable do
  begin
    Add(V);
  end;
end;

constructor THashSet<T>.Create(const AType: IType<T>);
begin
  { Call upper constructor }
  Create(AType, DefaultArrayLength);
end;

procedure THashSet<T>.DeserializeElement(const AElement: T);
begin
  { Simple as hell ... }
  Add(AElement);
end;

destructor THashSet<T>.Destroy;
begin
  { Clear first }
  Clear();

  inherited;
end;

function THashSet<T>.Empty: Boolean;
begin
  Result := (FCount = 0);
end;

function THashSet<T>.FindEntry(const AKey: T): Integer;
var
  HashCode : Integer;
  I        : Integer;
begin
  Result := -1;

  if Length(FBucketArray) > 0 then
  begin
    { Generate the hash code }
    HashCode := Hash(AKey);

    I := FBucketArray[HashCode mod Length(FBucketArray)];

    while I >= 0 do
    begin
      if (FEntryArray[I].FHashCode = HashCode) and ElementType.AreEqual(FEntryArray[I].FKey, AKey) then
         begin Result := I; Exit; end;

      I := FEntryArray[I].FNext;
    end;
  end;
end;

function THashSet<T>.GetCount: Cardinal;
begin
  Result := (FCount - FFreeCount);
end;

function THashSet<T>.GetEnumerator: IEnumerator<T>;
begin
  Result := THashSet<T>.TEnumerator.Create(Self);
end;

function THashSet<T>.Hash(const AKey: T): Integer;
const
  PositiveMask = not Integer($80000000);
begin
  Result := PositiveMask and ((PositiveMask and ElementType.GenerateHashCode(AKey)) + 1);
end;

procedure THashSet<T>.InitializeInternals(
  const Capacity: Cardinal);
var
  XPrime : Integer;
  I     : Integer;
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

procedure THashSet<T>.Insert(const AKey: T; const ShouldAdd: Boolean);
var
  FreeList : Integer;
  Index    : Integer;
  HashCode : Integer;
  I        : Integer;
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
    if (FEntryArray[I].FHashCode = HashCode) and ElementType.AreEqual(FEntryArray[I].FKey, AKey) then
    begin
      if (ShouldAdd) then
        ExceptionHelper.Throw_DuplicateKeyError('AKey');

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
  FEntryArray[FreeList].FNext := FBucketArray[Index];

  FBucketArray[Index] := FreeList;
  Inc(FVer);
end;

procedure THashSet<T>.Remove(const AValue: T);
var
  HashCode : Integer;
  Index    : Integer;
  I        : Integer;
  RemIndex : Integer;
begin
  if Length(FBucketArray) > 0 then
  begin
    { Generate the hash code }
    HashCode := Hash(AValue);

    Index := HashCode mod Length(FBucketArray);
    RemIndex := -1;

    I := FBucketArray[Index];

    while I >= 0 do
    begin
      if (FEntryArray[I].FHashCode = HashCode) and ElementType.AreEqual(FEntryArray[I].FKey, AValue) then
      begin

        if RemIndex < 0 then
        begin
          FBucketArray[Index] := FEntryArray[I].FNext;
        end else
        begin
          FEntryArray[RemIndex].FNext := FEntryArray[I].FNext;
        end;

        FEntryArray[I].FHashCode := -1;
        FEntryArray[I].FNext := FFreeList;
        FEntryArray[I].FKey := default(T);

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

procedure THashSet<T>.Resize;
var
  XPrime : Integer;
  I      : Integer;
  Index  : Integer;
  NArr   : TBucketArray;
begin
  XPrime := Prime.GetNearestProgressionPositive(FCount * 2);

  SetLength(NArr, XPrime);

  for I := 0 to Length(NArr) - 1 do
  begin
    NArr[I] := -1;
  end;

  SetLength(FEntryArray, XPrime);

  for I := 0 to FCount - 1 do
  begin
    Index := FEntryArray[I].FHashCode mod XPrime;
    FEntryArray[I].FNext := NArr[Index];
    NArr[Index] := I;
  end;

  { Reset bucket array }
  FBucketArray := nil;
  FBucketArray := NArr;
end;

procedure THashSet<T>.StartDeserializing(const AData: TDeserializationData);
begin
  // Do nothing, just say that I am here and I can be serialized
end;

procedure THashSet<T>.StartSerializing(const AData: TSerializationData);
begin
  // Do nothing, just say that I am here and I can be serialized
end;

{ THashSet<T>.HPairEnumerator }

constructor THashSet<T>.TEnumerator.Create(const ADict : THashSet<T>);
begin
  { Initialize }
  FDict := ADict;
  KeepObjectAlive(FDict);

  FCurrentIndex := 0;
  FVer := ADict.FVer;
end;

destructor THashSet<T>.TEnumerator.Destroy;
begin
  ReleaseObject(FDict);
  inherited;
end;

function THashSet<T>.TEnumerator.GetCurrent: T;
begin
  if FVer <> FDict.FVer then
    ExceptionHelper.Throw_CollectionChangedError();

  Result := FValue;
end;

function THashSet<T>.TEnumerator.MoveNext: Boolean;
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

constructor THashSet<T>.Create(const AArray: array of T);
begin
  Create(TType<T>.Default, AArray);
end;

constructor THashSet<T>.Create(const AType: IType<T>;
  const AArray: array of T);
var
  I : Integer;
begin
  { Call upper constructor }
  Create(AType, DefaultArrayLength);

  { Copy all in }
  for I := 0 to Length(AArray) - 1 do
  begin
    Add(AArray[I]);
  end;
end;

constructor THashSet<T>.Create(const AArray: TFixedArray<T>);
begin
  Create(TType<T>.Default, AArray);
end;

constructor THashSet<T>.Create(const AArray: TDynamicArray<T>);
begin
  Create(TType<T>.Default, AArray);
end;

constructor THashSet<T>.Create(const AType: IType<T>; const AArray: TFixedArray<T>);
var
  I: Cardinal;
begin
  { Call upper constructor }
  Create(AType, DefaultArrayLength);

  { Copy all items in }
  if AArray.Length > 0 then
    for I := 0 to AArray.Length - 1 do
    begin
      Add(AArray[I]);
    end;
end;

constructor THashSet<T>.Create(const AType: IType<T>; const AArray: TDynamicArray<T>);
var
  I: Cardinal;
begin
  { Call upper constructor }
  Create(AType, DefaultArrayLength);

  { Copy all items in }
  if AArray.Length > 0 then
    for I := 0 to AArray.Length - 1 do
    begin
      Add(AArray[I]);
    end;
end;

{ TObjectHashSet<T> }

procedure TObjectHashSet<T>.InstallType(const AType: IType<T>);
begin
  { Create a wrapper over the real type class and switch it }
  FWrapperType := TObjectWrapperType<T>.Create(AType);

  { Install overridden type }
  inherited InstallType(FWrapperType);
end;

function TObjectHashSet<T>.GetOwnsObjects: Boolean;
begin
  Result := FWrapperType.AllowCleanup;
end;

procedure TObjectHashSet<T>.SetOwnsObjects(const Value: Boolean);
begin
  FWrapperType.AllowCleanup := Value;
end;

end.
