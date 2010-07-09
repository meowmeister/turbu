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
unit DeHL.Arrays;
interface
uses SysUtils,
     DeHL.Base,
     DeHL.Types,
     DeHL.Serialization,
     DeHL.Exceptions;

type
  { Algorithms for dynamic arrays }
  &Array<T> = record
{$IFDEF OPTIMIZED_SORT}
  private type
    { Stack entry }
    TStackEntry = record
      First, Last: Integer;
    end;

    { Required for the non-recursive QSort }
    TQuickSortStack = array[0..63] of TStackEntry;
{$ENDIF}
  private
    class procedure QuickSort(var AArray: array of T; Left, Right: Integer; const AType: IType<T>; const Ascending: Boolean); static;

  public
    { Reversing }
    class procedure Reverse(var AArray: array of T); overload; static;
    class procedure Reverse(var AArray: array of T; const StartIndex, Count: Cardinal); overload; static;

    { Sorting }
    class procedure Sort(var AArray: array of T; const AType: IType<T>; const Ascending: Boolean = true); overload; static;
    class procedure Sort(var AArray: array of T; const StartIndex, Count: Cardinal; const AType: IType<T>; const Ascending: Boolean = true); overload; static;

    { Binary Searching }
    class function BinarySearch(var AArray: array of T; const Element: T; const AType: IType<T>; const Ascending: Boolean = true): Integer; overload; static;
    class function BinarySearch(var AArray: array of T; const Element: T; const StartIndex, Count: Cardinal; const AType: IType<T>; const Ascending: Boolean = true): Integer; overload; static;

    { Safe move }
    class procedure SafeMove(var SrcArray, DstArray: array of T; const SrcIndex, DstIndex, Count: Cardinal; const AType: IType<T>); static;

    { Variant arrays }
    class function ToVariantArray(const AArray: array of T; const AType: IType<T>): Variant; overload; static;
    class function ToVariantArray(const AArray: array of T): Variant; overload; static;
  end;

  { Fixed array definition }
  TFixedArray<T> = record
  private
    FArray: TArray<T>;

    { Getter functions }
    function GetItemAt(const Index: Cardinal): T; inline;
    function GetLength: Cardinal; inline;

{$HINTS OFF}
    class constructor Create();
    class destructor Destroy();
{$HINTS ON}
  public
    { Constructors }
    constructor Create(const AArray: array of T); overload;

    { Indexer }
    property Items[const Index: Cardinal]: T read GetItemAt; default;
    property Length: Cardinal read GetLength;

    { Generate variant arrays }
    function ToVariantArray(const AType: IType<T>): Variant; overload;
    function ToVariantArray(): Variant; overload;

    { Class properties }
    class function Consume(const AArray: TArray<T>): TFixedArray<T>; overload; static;
  end;

  { Dynamic array definition }
  TDynamicArray<T> = record
  private
    FArray: TArray<T>;

    { Set/Get functions }
    function GetItemAt(const Index: Cardinal): T; inline;
    procedure SetItemAt(const Index: Cardinal; const Value: T); inline;

    function GetLength: Cardinal; inline;
    procedure SetLength(const Value: Cardinal); inline;

{$HINTS OFF}
    class constructor Create();
    class destructor Destroy();
{$HINTS ON}
  public
    { Constructors }
    constructor Create(const AArray: array of T); overload;
    constructor Create(const AArray: TFixedArray<T>); overload;
    constructor Create(const InitialLength: Cardinal); overload;

    { Default Extend and Shrink }
    procedure Extend(const Count: Cardinal);
    procedure Shrink(const Count: Cardinal);

    { Combined add/remove + extend/shrink }
    procedure ExtendAndInsert(const AtIndex: Cardinal; const Element: T); overload;
    procedure ExtendAndInsert(const AtIndex: Cardinal; const Elements: array of T); overload;
    procedure ExtendAndInsert(const AtIndex: Cardinal; const Elements: TFixedArray<T>); overload;
    procedure ExtendAndInsert(const AtIndex: Cardinal; const Elements: TDynamicArray<T>); overload;

    function RemoveAndShrink(const FromIndex: Cardinal): T;

    { Simple remove and add (note: can throw exceptions) }
    procedure Insert(const AtIndex: Cardinal; const Element: T); overload;
    procedure Insert(const AtIndex: Cardinal; const Elements: array of T); overload;
    procedure Insert(const AtIndex: Cardinal; const Elements: TFixedArray<T>); overload;
    procedure Insert(const AtIndex: Cardinal; const Elements: TDynamicArray<T>); overload;

    function Remove(const FromIndex: Cardinal): T;

    { Appending }
    procedure Append(const Element: T); overload;
    procedure Append(const Elements: array of T); overload;
    procedure Append(const Elements: TFixedArray<T>); overload;
    procedure Append(const Elements: TDynamicArray<T>); overload;

    { Filling }
    procedure Fill(const Element: T); overload;
    procedure Fill(const StartIndex, Count: Cardinal; const Element: T); overload;

    { Disposing }
    procedure Dispose(); overload;

    { Reversing }
    procedure Reverse(); overload;
    procedure Reverse(const StartIndex, Count: Cardinal); overload;

    { Sorting }
    procedure Sort(const AType: IType<T>; const Ascending: Boolean = true); overload;
    procedure Sort(const StartIndex, Count: Cardinal; const AType: IType<T>; const Ascending: Boolean = true); overload;

    { Binary Searching }
    function BinarySearch(const Element: T; const AType: IType<T>; const Ascending: Boolean = true): Integer; overload;
    function BinarySearch(const Element: T; const StartIndex, Count: Cardinal; const AType: IType<T>; const Ascending: Boolean = true): Integer; overload;

    { Indexer }
    property Items[const Index: Cardinal]: T read GetItemAt write SetItemAt; default;
    property Length: Cardinal read GetLength write SetLength;

    { Convert to a variant array }
    function ToVariantArray(const AType: IType<T>): Variant; overload;
    function ToVariantArray(): Variant; overload;

    class function Consume(const AArray: TArray<T>): TDynamicArray<T>; overload; static;

    { To fixed array }
    function ToFixedArray(): TFixedArray<T>;
  end;

  { Binary Support }
  TFixedArrayType<T> = class sealed(TMagicType<TFixedArray<T>>)
  private
    FType: IType<TArray<T>>;

  protected
    { Serialization }
    procedure DoSerialize(const AInfo: TValueInfo; const AValue: TFixedArray<T>; const AContext: ISerializationContext); override;
    procedure DoDeserialize(const AInfo: TValueInfo; out AValue: TFixedArray<T>; const AContext: IDeserializationContext); override;

  public
    { Comparator }
    function Compare(const AValue1, AValue2: TFixedArray<T>): Integer; override;

    { Equality Comparator }
    function AreEqual(const AValue1, AValue2: TFixedArray<T>): Boolean; override;

    { Hash code provider }
    function GenerateHashCode(const AValue: TFixedArray<T>): Integer; override;

    { Get String representation }
    function GetString(const AValue: TFixedArray<T>): String; override;

    { Type information }
    function Family(): TTypeFamily; override;

    { Variant Conversion }
    function TryConvertToVariant(const AValue: TFixedArray<T>; out ORes: Variant): Boolean; override;
    function TryConvertFromVariant(const AValue: Variant; out ORes: TFixedArray<T>): Boolean; override;

    { Constructor }
    constructor Create(); overload; override;
    constructor Create(const AArrayType: IType<TArray<T>>); reintroduce; overload;
  end;

  { Binary Support }
  TDynamicArrayType<T> = class sealed(TMagicType<TDynamicArray<T>>)
  private
    FType: IType<TArray<T>>;

  protected
    { Serialization }
    procedure DoSerialize(const AInfo: TValueInfo; const AValue: TDynamicArray<T>; const AContext: ISerializationContext); override;
    procedure DoDeserialize(const AInfo: TValueInfo; out AValue: TDynamicArray<T>; const AContext: IDeserializationContext); override;

  public
    { Comparator }
    function Compare(const AValue1, AValue2: TDynamicArray<T>): Integer; override;

    { Equality Comparator }
    function AreEqual(const AValue1, AValue2: TDynamicArray<T>): Boolean; override;

    { Hash code provider }
    function GenerateHashCode(const AValue: TDynamicArray<T>): Integer; override;

    { Get String representation }
    function GetString(const AValue: TDynamicArray<T>): String; override;

    { Type information }
    function Family(): TTypeFamily; override;

    { Variant Conversion }
    function TryConvertToVariant(const AValue: TDynamicArray<T>; out ORes: Variant): Boolean; override;
    function TryConvertFromVariant(const AValue: Variant; out ORes: TDynamicArray<T>): Boolean; override;

    { Constructor }
    constructor Create(); overload; override;
    constructor Create(const AArrayType: IType<TArray<T>>); reintroduce; overload;
  end;

implementation
uses Variants;

{ Disable overflow and range checks }
{$Q-}
{$R-}

{ TFixedArray<T> }

class constructor TFixedArray<T>.Create();
begin
  { Register custom type }
  if not TType<TFixedArray<T>>.IsRegistered then
    TType<TFixedArray<T>>.Register(TFixedArrayType<T>);
end;

class destructor TFixedArray<T>.Destroy();
begin
  { Unregister the custom type }
  if not TType<TFixedArray<T>>.IsRegistered then
    TType<TFixedArray<T>>.Unregister();
end;

function TFixedArray<T>.GetItemAt(const Index: Cardinal): T;
begin
  if Index >= Cardinal(System.Length(FArray)) then
     ExceptionHelper.Throw_ArgumentOutOfRangeError('Index');

  Result := FArray[Index];
end;

function TFixedArray<T>.GetLength: Cardinal;
begin
  Result := Cardinal(System.Length(FArray));
end;

class function TFixedArray<T>.Consume(const AArray: TArray<T>): TFixedArray<T>;
begin
  { Consume the array directly }
  Result.FArray := AArray;
end;

constructor TFixedArray<T>.Create(const AArray: array of T);
var
  I: Integer;
begin
  { Copy all internals }
  SetLength(FArray, System.Length(AArray));

  for I := 0 to System.Length(AArray) - 1 do
  begin
    FArray[I] := AArray[I];
  end;
end;

function TFixedArray<T>.ToVariantArray(const AType: IType<T>): Variant;
begin
  { Call helper }
  Result := &Array<T>.ToVariantArray(FArray, AType);
end;

function TFixedArray<T>.ToVariantArray(): Variant;
begin
  { Call helper }
  Result := &Array<T>.ToVariantArray(FArray);
end;

{ TDynamicArray<T> }

procedure TDynamicArray<T>.Append(const Element: T);
var
  PrevLen: Integer;
begin
  { +1 length and add last element }
  PrevLen := System.Length(FArray);
  System.SetLength(FArray, PrevLen + 1);
  FArray[PrevLen] := Element;
end;

procedure TDynamicArray<T>.Append(const Elements: array of T);
var
  PrevLen, I: Integer;
begin
  { +N length and add last element }
  PrevLen := System.Length(FArray);
  System.SetLength(FArray, PrevLen + System.Length(Elements));

  { Copy the elements in }
  for I := 0 to System.Length(Elements) - 1 do
    FArray[PrevLen + I] := Elements[I];
end;

procedure TDynamicArray<T>.Append(const Elements: TFixedArray<T>);
var
  PrevLen, I: Integer;
begin
  { +N length and add last element }
  PrevLen := System.Length(FArray);
  System.SetLength(FArray, PrevLen + System.Length(Elements.FArray));

  { Copy the elements in }
  for I := 0 to System.Length(Elements.FArray) - 1 do
    FArray[PrevLen + I] := Elements.FArray[I];
end;

procedure TDynamicArray<T>.Append(const Elements: TDynamicArray<T>);
var
  PrevLen, I: Integer;
begin
  { +N length and add last element }
  PrevLen := System.Length(FArray);
  System.SetLength(FArray, PrevLen + System.Length(Elements.FArray));

  { Copy the elements in }
  for I := 0 to System.Length(Elements.FArray) - 1 do
    FArray[PrevLen + I] := Elements.FArray[I];
end;

function TDynamicArray<T>.BinarySearch(const Element: T;
  const AType: IType<T>; const Ascending: Boolean): Integer;
begin
  { call the more generic function }
  Result := &Array<T>.BinarySearch(FArray, Element, AType, Ascending);
end;

function TDynamicArray<T>.BinarySearch(const Element: T; const StartIndex,
  Count: Cardinal; const AType: IType<T>; const Ascending: Boolean): Integer;
begin
  Result := &Array<T>.BinarySearch(FArray, Element, StartIndex, Count, AType, Ascending);
end;

class constructor TDynamicArray<T>.Create();
begin
  { Register custom type }
  if not TType<TDynamicArray<T>>.IsRegistered then
    TType<TDynamicArray<T>>.Register(TDynamicArrayType<T>);
end;

class destructor TDynamicArray<T>.Destroy();
begin
  { Unregister the custom type }
  if not TType<TDynamicArray<T>>.IsRegistered then
    TType<TDynamicArray<T>>.Unregister();
end;

constructor TDynamicArray<T>.Create(const InitialLength: Cardinal);
begin
  { Set length }
  System.SetLength(FArray, InitialLength);
end;

class function TDynamicArray<T>.Consume(const AArray: TArray<T>): TDynamicArray<T>;
begin
  { Consume the array directly }
  Result.FArray := AArray;
end;

procedure TDynamicArray<T>.Dispose;
begin
  { Simply reset the new length }
  System.SetLength(FArray, 0);
end;

procedure TDynamicArray<T>.Extend(const Count: Cardinal);
var
  PrevLen: Cardinal;
begin
  { +1 length and add last element }
  PrevLen := System.Length(FArray);
  System.SetLength(FArray, PrevLen + Count);
end;

procedure TDynamicArray<T>.ExtendAndInsert(const AtIndex: Cardinal; const Element: T);
var
  PrevLen, I: Cardinal;
begin
  if AtIndex > Cardinal(System.Length(FArray)) then
     ExceptionHelper.Throw_ArgumentOutOfRangeError('AtIndex');

  { +1 length  }
  PrevLen := System.Length(FArray);
  System.SetLength(FArray, PrevLen + 1);

  { Move to the right }
  for I := PrevLen downto AtIndex do
      FArray[I] := FArray[I - 1];

  { Insert }
  FArray[AtIndex] := Element;
end;

procedure TDynamicArray<T>.ExtendAndInsert(const AtIndex: Cardinal; const Elements: array of T);
var
  PrevLen, I, L: Cardinal;
begin
  if AtIndex > Cardinal(System.Length(FArray)) then
     ExceptionHelper.Throw_ArgumentOutOfRangeError('AtIndex');

  { +N length }
  L := System.Length(Elements);

  { Do nothing on 0 }
  if L = 0 then
    Exit;

  PrevLen := System.Length(FArray);
  System.SetLength(FArray, PrevLen + L);

  { Move to the right }
  if PrevLen <> 0 then
    for I := PrevLen - 1 downto AtIndex do
      FArray[I + L] := FArray[I];

  { Insert list }
  for I := 0 to L - 1 do
    FArray[AtIndex + I] := Elements[I];
end;

procedure TDynamicArray<T>.ExtendAndInsert(const AtIndex: Cardinal; const Elements: TFixedArray<T>);
var
  PrevLen, I, L: Cardinal;
begin
  if AtIndex > Cardinal(System.Length(FArray)) then
     ExceptionHelper.Throw_ArgumentOutOfRangeError('AtIndex');

  { +N length }
  L := System.Length(Elements.FArray);

  { Do nothing on 0 }
  if L = 0 then
    Exit;

  PrevLen := System.Length(FArray);
  System.SetLength(FArray, PrevLen + L);

  { Move to the right }
  if PrevLen <> 0 then
    for I := PrevLen - 1 downto AtIndex do
      FArray[I + L] := FArray[I];

  { Insert list }
  for I := 0 to L - 1 do
    FArray[AtIndex + I] := Elements.FArray[I];
end;

procedure TDynamicArray<T>.ExtendAndInsert(const AtIndex: Cardinal; const Elements: TDynamicArray<T>);
var
  PrevLen, I, L: Cardinal;
begin
  if AtIndex > Cardinal(System.Length(FArray)) then
     ExceptionHelper.Throw_ArgumentOutOfRangeError('AtIndex');

  { +N length }
  L := System.Length(Elements.FArray);

  { Do nothing on 0 }
  if L = 0 then
    Exit;

  PrevLen := System.Length(FArray);
  System.SetLength(FArray, PrevLen + L);

  { Move to the right }
  if PrevLen <> 0 then
    for I := PrevLen - 1 downto AtIndex do
      FArray[I + L] := FArray[I];

  { Insert list }
  for I := 0 to L - 1 do
    FArray[AtIndex + I] := Elements.FArray[I];
end;

procedure TDynamicArray<T>.Fill(const Element: T);
begin
  { Call more generic function }
  Fill(0, System.Length(FArray), Element);
end;

procedure TDynamicArray<T>.Fill(const StartIndex, Count: Cardinal;
  const Element: T);
var
  I: Cardinal;
begin
  if (StartIndex + Count) > Cardinal(System.Length(FArray)) then
     ExceptionHelper.Throw_ArgumentOutOfRangeError('StartIndex/Count');

  { Simple fill }
  for I := StartIndex to (StartIndex + Count) - 1 do
      FArray[I] := Element;
end;

function TDynamicArray<T>.GetItemAt(const Index: Cardinal): T;
begin
  if Index >= Cardinal(System.Length(FArray)) then
     ExceptionHelper.Throw_ArgumentOutOfRangeError('Index');

  Result := FArray[Index];
end;

function TDynamicArray<T>.GetLength: Cardinal;
begin
  Result := Cardinal(System.Length(FArray));
end;

procedure TDynamicArray<T>.Insert(const AtIndex: Cardinal; const Element: T);
var
  I: Cardinal;
begin
  if AtIndex >= Cardinal(System.Length(FArray)) then
     ExceptionHelper.Throw_ArgumentOutOfRangeError('AtIndex');

  { Move to the right }
  for I := Cardinal(System.Length(FArray)) - 1 downto AtIndex do
      FArray[I] := FArray[I - 1];

  { Insert }
  FArray[AtIndex] := Element;
end;

procedure TDynamicArray<T>.Insert(const AtIndex: Cardinal; const Elements: array of T);
var
  I, L, Len: Cardinal;
begin
  Len := Cardinal(System.Length(FArray));

  if AtIndex >= Len then
     ExceptionHelper.Throw_ArgumentOutOfRangeError('AtIndex');

  L := Cardinal(System.Length(Elements));

  if L = 0 then
    Exit;

  { Move to the right }
  if L < (Len - AtIndex) then
    for I := Len - 1 downto AtIndex do
      FArray[I] := FArray[I - L];

  { Insert elements }
  for I := 0 to L - 1 do
  begin
    { Do not continue past array boundaries }
    if I + AtIndex >= Len then
      break;

    FArray[AtIndex + I] := Elements[I];
  end;
end;

procedure TDynamicArray<T>.Insert(const AtIndex: Cardinal; const Elements: TFixedArray<T>);
var
  I, L, Len: Cardinal;
begin
  Len := Cardinal(System.Length(FArray));

  if AtIndex >= Len then
     ExceptionHelper.Throw_ArgumentOutOfRangeError('AtIndex');

  L := Cardinal(System.Length(Elements.FArray));

  if L = 0 then
    Exit;

  { Move to the right }
  if L < (Len - AtIndex) then
    for I := Len - 1 downto AtIndex do
      FArray[I] := FArray[I - L];

  { Insert elements }
  for I := 0 to L - 1 do
  begin
    { Do not continue past array boundaries }
    if I + AtIndex >= Len then
      break;

    FArray[AtIndex + I] := Elements.FArray[I];
  end;
end;

procedure TDynamicArray<T>.Insert(const AtIndex: Cardinal; const Elements: TDynamicArray<T>);
var
  I, L, Len: Cardinal;
begin
  Len := Cardinal(System.Length(FArray));

  if AtIndex >= Len then
     ExceptionHelper.Throw_ArgumentOutOfRangeError('AtIndex');

  L := Cardinal(System.Length(Elements.FArray));

  if L = 0 then
    Exit;

  { Move to the right }
  if L < (Len - AtIndex) then
    for I := Len - 1 downto AtIndex do
      FArray[I] := FArray[I - L];

  { Insert elements }
  for I := 0 to L - 1 do
  begin
    { Do not continue past array boundaries }
    if I + AtIndex >= Len then
      break;

    FArray[AtIndex + I] := Elements.FArray[I];
  end;
end;

function TDynamicArray<T>.Remove(const FromIndex: Cardinal): T;
var
  I  : Cardinal;
  Len: Cardinal;
begin
  Len := System.Length(FArray);

  if FromIndex >= Len then
     ExceptionHelper.Throw_ArgumentOutOfRangeError('FromIndex');

  Result := FArray[FromIndex];

  for I := FromIndex to Len - 2 do
      FArray[I] := FArray[I + 1];
end;

function TDynamicArray<T>.RemoveAndShrink(const FromIndex: Cardinal): T;
var
  I  : Cardinal;
  Len: Cardinal;
begin
  Len := System.Length(FArray);

  if FromIndex >= Len then
     ExceptionHelper.Throw_ArgumentOutOfRangeError('FromIndex');

  Result := FArray[FromIndex];

  for I := FromIndex to Len - 2 do
      FArray[I] := FArray[I + 1];

  System.SetLength(FArray, Len - 1);
end;

procedure TDynamicArray<T>.Reverse;
begin
  &Array<T>.Reverse(FArray);
end;

procedure TDynamicArray<T>.Reverse(const StartIndex, Count: Cardinal);
begin
  &Array<T>.Reverse(FArray, StartIndex, Count);
end;

procedure TDynamicArray<T>.SetItemAt(const Index: Cardinal; const Value: T);
begin
  if Index >= Cardinal(System.Length(FArray)) then
     ExceptionHelper.Throw_ArgumentOutOfRangeError('Index');

  FArray[Index] := Value;
end;

procedure TDynamicArray<T>.SetLength(const Value: Cardinal);
begin
  { Simply call the system }
  System.SetLength(FArray, Value);
end;

procedure TDynamicArray<T>.Shrink(const Count: Cardinal);
var
  PrevLen: Cardinal;
begin
  { -1 length and add last element }
  PrevLen := System.Length(FArray);

  if Count > PrevLen then
    ExceptionHelper.Throw_ArgumentOutOfRangeError('Count');

  System.SetLength(FArray, PrevLen - Count);
end;

procedure TDynamicArray<T>.Sort(const AType: IType<T>; const Ascending: Boolean);
begin
  { Call the more generic variant }
  &Array<T>.Sort(FArray, AType, Ascending);
end;

procedure TDynamicArray<T>.Sort(const StartIndex, Count: Cardinal;
  const AType: IType<T>; const Ascending: Boolean);
begin
  &Array<T>.Sort(FArray, StartIndex, Count, AType, Ascending);
end;

function TDynamicArray<T>.ToFixedArray: TFixedArray<T>;
begin
  { Make a copy }
  Result := TFixedArray<T>.Create(FArray);
end;

function TDynamicArray<T>.ToVariantArray(const AType: IType<T>): Variant;
begin
  { Call helper }
  Result := &Array<T>.ToVariantArray(FArray, AType);
end;

function TDynamicArray<T>.ToVariantArray(): Variant;
begin
  { Call helper }
  Result := &Array<T>.ToVariantArray(FArray);
end;

constructor TDynamicArray<T>.Create(const AArray: TFixedArray<T>);
var
  I: Integer;
begin
  { Copy all internals }
  Length := AArray.Length;

  for I := 0 to Length - 1 do
  begin
    FArray[I] := AArray[I];
  end;
end;

constructor TDynamicArray<T>.Create(const AArray: array of T);
var
  I: Integer;
begin
  { Copy all internals }
  Length := System.Length(AArray);

  for I := 0 to Length - 1 do
  begin
    FArray[I] := AArray[I];
  end;
end;


{ &Array<T> }

class function &Array<T>.BinarySearch(var AArray: array of T;
  const Element: T; const StartIndex, Count: Cardinal;
  const AType: IType<T>; const Ascending: Boolean): Integer;
var
  Left, Right, Middle: Integer;
  CompareResult      : Integer;
begin
  if (StartIndex + Count) > Cardinal(System.Length(AArray)) then
     ExceptionHelper.Throw_ArgumentOutOfRangeError('StartIndex/Count');

  if AType = nil then
     ExceptionHelper.Throw_ArgumentNilError('AType');

  { Do not search for 0 count }
  if Count = 0 then
  begin
    Result := -1;
    Exit;
  end;

  { Check for valid type support }
  Left := StartIndex;
  Right := Cardinal(Left) + Count - 1;

  while (Left <= Right) do
  begin
    Middle := (Left + Right) div 2;
    CompareResult := AType.Compare(AArray[Middle], Element);

    if not Ascending then
       CompareResult := CompareResult * -1;

    if CompareResult > 0 then
      Right := Middle - 1
    else if CompareResult < 0 then
       Left := Middle + 1
    else
       begin Result := Cardinal(Middle) - StartIndex; Exit; end;
  end;

  Result := -1;
end;

{$IFNDEF OPTIMIZED_SORT}
class procedure &Array<T>.QuickSort(var AArray: array of T;
  Left, Right: Integer; const AType: IType<T>;
  const Ascending: Boolean);
var
  I, J: Integer;
  Pivot, Temp: T;
  SignFix: Integer;
begin
  ASSERT(AType <> nil);
  ASSERT(Left <= Right);

  if Ascending then
    SignFix := 1
  else
    SignFix := -1;

  repeat
    I := Left;
    J := Right;

    Pivot := AArray[(Left + Right) div 2];

    repeat
      while (AType.Compare(AArray[I], Pivot) * SignFix) < 0 do
        Inc(I);

      while (AType.Compare(AArray[J], Pivot) * SignFix) > 0 do
        Dec(J);

      if I <= J then
      begin

        if I <> J then
        begin
          Temp := AArray[I];
          AArray[I] := AArray[J];
          AArray[J] := Temp;
        end;

        Inc(I);
        Dec(J);
      end;

    until I > J;

    if Left < J then
      QuickSort(AArray, Left, J, AType, Ascending);

    Left := I;

  until I >= Right;
end;

{$ELSE}

class procedure &Array<T>.QuickSort(var AArray: array of T;
  Left, Right: Integer; const AType: IType<T>;
  const Ascending: Boolean);
var
  SubArray, SubLeft, SubRight: integer;
  Pivot, Temp: T;
  SignFix: Integer;
  Stack: TQuickSortStack;
begin
  ASSERT(AType <> nil);
  ASSERT(Left <= Right);

  SubArray := 0;

  Stack[SubArray].First := Left;
  Stack[SubArray].Last := Right;

  if Ascending then
    SignFix := 1
  else
    SignFix := -1;

  repeat
    Left  := Stack[SubArray].First;
    Right := Stack[SubArray].Last;
    Dec(SubArray);
    repeat
      SubLeft := Left;
      SubRight := Right;
      Pivot:= AArray[(Left + Right) shr 1];

      repeat
        while (AType.Compare(AArray[SubLeft], Pivot) * SignFix) < 0 do
          Inc(SubLeft);

        while (AType.Compare(AArray[SubRight], Pivot) * SignFix) > 0 do
          Dec(SubRight);

        if SubLeft <= SubRight then
        begin
          Temp := AArray[SubLeft];
          AArray[SubLeft] := AArray[SubRight];
          AArray[SubRight] := Temp;
          Inc(SubLeft);
          Dec(SubRight);
        end;
      until SubLeft > SubRight;

      if SubLeft < Right then
      begin
        Inc(SubArray);
        Stack[SubArray].First := SubLeft;
        Stack[SubArray].Last  := Right;
      end;

      Right := SubRight;
    until Left >= Right;
  until SubArray < 0;
end;
{$ENDIF}

class function &Array<T>.BinarySearch(var AArray: array of T;
  const Element: T; const AType: IType<T>;
  const Ascending: Boolean): Integer;
begin
  { call the more generic function }
  Result := BinarySearch(AArray, Element, 0, Length(AArray), AType, Ascending);
end;

class procedure &Array<T>.Reverse(var AArray: array of T;
  const StartIndex, Count: Cardinal);
var
  I : Cardinal;
  V : T;
begin
  { Check for indexes }
  if ((StartIndex + Count) > Cardinal(Length(AArray))) then
     ExceptionHelper.Throw_ArgumentOutOfRangeError('StartIndex/Count');

  if Count < 2 then
     Exit;

  { Reverse the array }
  for I := 0 to (Count div 2) - 1 do
  begin
    V := AArray[StartIndex + I];
    AArray[StartIndex + I] := AArray[StartIndex + Count - I - 1];
    AArray[StartIndex + Count - I - 1] := V;
  end;
end;

class procedure &Array<T>.Reverse(var AArray: array of T);
begin
  { Call the more generic function }
  Reverse(AArray, 0, Length(AArray));
end;

class procedure &Array<T>.Sort(var AArray: array of T;
  const AType: IType<T>; const Ascending: Boolean);
begin
  { Call the more generic variant }
  Sort(AArray, 0, Length(AArray), AType, Ascending);
end;

class procedure &Array<T>.SafeMove(var SrcArray, DstArray: array of T;
    const SrcIndex, DstIndex, Count: Cardinal; const AType: IType<T>);
var
  I: Cardinal;
begin
  { Do not check for indexes for performance reasons }
  if AType.Management() = tmCompiler then
  begin
    { Copy - using compiler provided magic }
    for I := 0 to Count - 1 do
      DstArray[I + DstIndex] := SrcArray[I + SrcIndex];
  end else
  begin
    { Move directly }
    Move(SrcArray[SrcIndex], DstArray[DstIndex], Count * SizeOf(T));
  end;
end;

class procedure &Array<T>.Sort(var AArray: array of T;
  const StartIndex, Count: Cardinal; const AType: IType<T>;
  const Ascending: Boolean);
begin
  { Check for indexes }
  if ((StartIndex + Count) > Cardinal(System.Length(AArray))) then
     ExceptionHelper.Throw_ArgumentOutOfRangeError('StartIndex/Count');

  if AType = nil then
     ExceptionHelper.Throw_ArgumentNilError('AType');

  if Count < 2 then
     Exit;

  { Start quick sort }
  QuickSort(AArray, StartIndex, (StartIndex + Count) - 1, AType, Ascending);
end;

class function &Array<T>.ToVariantArray(const AArray: array of T; const AType: IType<T>): Variant;
var
  LVariantType, LOrigType: Word;
  Indices: array[0..0] of Integer;
  I: Cardinal;
begin
  { Check arguments }
  if AType = nil then
    ExceptionHelper.Throw_ArgumentNilError('AType');

  Result := Unassigned;

  { Try to create a variant array }
  try
    LOrigType := VarType(AType.ConvertToVariant(default(T)));

    { Modify the Variant type in case it is String or Unicode string which are Delphi only }
    if (LOrigType = varString) or (LOrigType = varUString) then
      LVariantType := varOleStr
    else
      LVariantType := LOrigType;

    Result := VarArrayCreate([0, Length(AArray) - 1], LVariantType);
  except
    ExceptionHelper.Throw_TypeIncompatibleWithVariantArray(AType.Name);
  end;

  { And populate the variant array }
  if Length(AArray) > 0 then
  begin
    if LVariantType <> LOrigType then
    begin
      { Copy the array un-altered }
      for I := 0 to Length(AArray) - 1 do
      begin
        Indices[0] := I;
        VarArrayPut(Result, AType.ConvertToVariant(AArray[I]), Indices);
      end;
    end else
    begin
      { Copy the array with alteration rules }
      for I := 0 to Length(AArray) - 1 do
      begin
        Indices[0] := I;
        VarArrayPut(Result, VarAsType(AType.ConvertToVariant(AArray[I]), LVariantType), Indices);
      end;
    end;
  end;
end;

class function &Array<T>.ToVariantArray(const AArray: array of T): Variant;
begin
  { Call upper function }
  Result := ToVariantArray(AArray, TType<T>.Default);
end;

{ TFixedArrayType<T> }

function TFixedArrayType<T>.AreEqual(const AValue1, AValue2: TFixedArray<T>): Boolean;
begin
  { Pass-through }
  Result := FType.AreEqual(AValue1.FArray, AValue2.FArray);
end;

constructor TFixedArrayType<T>.Create();
begin
  inherited;

  { Obtain the type }
  FType := TType<TArray<T>>.Default;
end;

constructor TFixedArrayType<T>.Create(const AArrayType: IType<TArray<T>>);
begin
  inherited Create();

  if AArrayType = nil then
    ExceptionHelper.Throw_ArgumentNilError('AArrayType');

  FType := AArrayType;
end;

function TFixedArrayType<T>.Compare(const AValue1, AValue2: TFixedArray<T>): Integer;
begin
  { Pass-through }
  Result := FType.Compare(AValue1.FArray, AValue2.FArray);
end;

procedure TFixedArrayType<T>.DoSerialize(const AInfo: TValueInfo; const AValue: TFixedArray<T>; const AContext: ISerializationContext);
begin
  { Pass-through }
  FType.Serialize(AInfo, AValue.FArray, AContext);
end;

procedure TFixedArrayType<T>.DoDeserialize(const AInfo: TValueInfo; out AValue: TFixedArray<T>; const AContext: IDeserializationContext);
begin
  { Pass-through }
  FType.Deserialize(AInfo, AValue.FArray, AContext);
end;

function TFixedArrayType<T>.Family: TTypeFamily;
begin
  Result := tfArray;
end;

function TFixedArrayType<T>.GenerateHashCode(const AValue: TFixedArray<T>): Integer;
begin
  { Pass-through }
  Result := FType.GenerateHashCode(AValue.FArray);
end;

function TFixedArrayType<T>.GetString(const AValue: TFixedArray<T>): String;
begin
  { Pass-through }
  Result := FType.GetString(AValue.FArray);
end;

function TFixedArrayType<T>.TryConvertToVariant(const AValue: TFixedArray<T>; out ORes: Variant): Boolean;
begin
  { Pass-through }
  Result := FType.TryConvertToVariant(AValue.FArray, ORes);
end;

function TFixedArrayType<T>.TryConvertFromVariant(const AValue: Variant; out ORes: TFixedArray<T>): Boolean;
begin
  { Pass-through }
  Result := FType.TryConvertFromVariant(AValue, ORes.FArray);
end;

{ TDynamicArrayType<T> }

function TDynamicArrayType<T>.AreEqual(const AValue1, AValue2: TDynamicArray<T>): Boolean;
begin
  { Pass-through }
  Result := FType.AreEqual(AValue1.FArray, AValue2.FArray);
end;

constructor TDynamicArrayType<T>.Create();
begin
  inherited;

  { Obtain the type }
  FType := TType<TArray<T>>.Default;
end;

constructor TDynamicArrayType<T>.Create(const AArrayType: IType<TArray<T>>);
begin
  inherited Create();

  if AArrayType = nil then
    ExceptionHelper.Throw_ArgumentNilError('AArrayType');

  FType := AArrayType;
end;

function TDynamicArrayType<T>.Compare(const AValue1, AValue2: TDynamicArray<T>): Integer;
begin
  { Pass-through }
  Result := FType.Compare(AValue1.FArray, AValue2.FArray);
end;

procedure TDynamicArrayType<T>.DoSerialize(const AInfo: TValueInfo; const AValue: TDynamicArray<T>; const AContext: ISerializationContext);
begin
  { Pass-through }
  FType.Serialize(AInfo, AValue.FArray, AContext);
end;

procedure TDynamicArrayType<T>.DoDeserialize(const AInfo: TValueInfo; out AValue: TDynamicArray<T>; const AContext: IDeserializationContext);
begin
  { Pass-through }
  FType.Deserialize(AInfo, AValue.FArray, AContext);
end;

function TDynamicArrayType<T>.Family: TTypeFamily;
begin
  Result := tfArray;
end;

function TDynamicArrayType<T>.GenerateHashCode(const AValue: TDynamicArray<T>): Integer;
begin
  { Pass-through }
  Result := FType.GenerateHashCode(AValue.FArray);
end;

function TDynamicArrayType<T>.GetString(const AValue: TDynamicArray<T>): String;
begin
  { Pass-through }
  Result := FType.GetString(AValue.FArray);
end;

function TDynamicArrayType<T>.TryConvertToVariant(const AValue: TDynamicArray<T>; out ORes: Variant): Boolean;
begin
  { Pass-through }
  Result := FType.TryConvertToVariant(AValue.FArray, ORes);
end;

function TDynamicArrayType<T>.TryConvertFromVariant(const AValue: Variant; out ORes: TDynamicArray<T>): Boolean;
begin
  { Pass-through }
  Result := FType.TryConvertFromVariant(AValue, ORes.FArray);
end;



end.

