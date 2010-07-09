(*
* Copyright (c) 2009-2010, Ciobanu Alexandru
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
unit DeHL.Box;
interface
uses
  SysUtils,
  DeHL.Base,
  DeHL.Types,
  DeHL.StrConsts,
  DeHL.Serialization,
  DeHL.Exceptions;

type
  { The non-generic box interface }
  IBox = interface
    { Verification }
    function HasBoxedValue(): Boolean;

    { Comparability/Equatability }
    function CompareTo(Obj: TObject): Integer; overload;
    function Equals(Obj: TObject): Boolean; overload;

    { Hash code: gets the hashcode of the boxed }
    function GetHashCode(): Integer;

    { ToString }
    function ToString(): String;
  end;

  { The box interface }
  IBox<T> = interface(IBox)
    { Used to get the value from the box but not "unbox it" }
    function TryPeek(out AValue: T): Boolean;
    function Peek(): T;

    { Used to get the value from the box and consider it unboxed }
    function TryUnbox(out AValue: T): Boolean;
    function Unbox(): T;

    { Comparability/Equatability }
    function CompareTo(Value: T): Integer; overload;
    function Equals(Value: T): Boolean; overload;
  end;

  { A non-generic class base class for the boxes }
  TBox = class abstract(TRefCountedObject, IBox, IComparable, ISerializable)
  protected
    { Serialization support }
    procedure Serialize(const AData: TSerializationData); virtual; abstract;
    procedure Deserialize(const AData: TDeserializationData); virtual; abstract;
  public
    { Verify that this value contains something }
    function HasBoxedValue(): Boolean; virtual; abstract;

    { IComparable }
    function CompareTo(Obj: TObject): Integer; virtual; abstract;
  end;

  { A boxed value, an object containing a value }
  TBox<T> = class sealed(TBox, IBox<T>, IComparable<T>, IEquatable<T>)
  private
    FValue: T;
    FIsBoxed: Boolean;
    FType: IType<T>;

  protected
    { Serialization support }
    procedure Serialize(const AData: TSerializationData); override;
    procedure Deserialize(const AData: TDeserializationData); override;

  public
    { Costructors }
    constructor Create(const AType: IType<T>; const AValue: T); overload;
    constructor Create(const AValue: T); overload;
    constructor Create(const AType: IType<T>); overload;
    constructor Create(); overload;

    { Verify that this value contains something }
    function HasBoxedValue(): Boolean; override;

    { Value peeking }
    function TryPeek(out AValue: T): Boolean; inline;
    function Peek(): T; inline;

    { Value unboxing }
    function TryUnbox(out AValue: T): Boolean; inline;
    function Unbox(): T; inline;
    function UnboxAndFree(): T; inline;

    { IComparable and IEquatable }
    function CompareTo(Obj: TObject): Integer; overload; override;
    function CompareTo(Value: T): Integer; reintroduce; overload; inline;
    function Equals(Value: T): Boolean; reintroduce; overload; inline;

    { Normal overrides }
    function Equals(Obj: TObject): Boolean; overload; override;
    function GetHashCode(): Integer; override;
    function ToString(): String; override;

    { Destructor }
    destructor Destroy(); override;
  end;

 { TBox class Support }
  TBoxType<T: TBox> = class sealed(TClassType<T>)
  public
    { Comparator }
    function Compare(const AValue1, AValue2: T): Integer; override;
    constructor Create(); overload; override;
  end;


implementation

{ TBox<T> }

constructor TBox<T>.Create(const AValue: T);
begin
  { Call upper constructor }
  Create(TType<T>.Default, AValue);
end;

function TBox<T>.CompareTo(Value: T): Integer;
begin
  { Verify that we actually have a boxed value }
  if not FIsBoxed then
    ExceptionHelper.Throw_TheBoxIsEmpty();

  { Use the provided type class }
  Result := FType.Compare(FValue, Value);
end;

function TBox<T>.CompareTo(Obj: TObject): Integer;
begin
  if not (Obj is TBox<T>) then
    ExceptionHelper.Throw_ArgumentNotSameTypeError('Obj');

  { Verify that we actually have a boxed value }
  if not FIsBoxed then
    ExceptionHelper.Throw_TheBoxIsEmpty();

  { Use the provided type class }
  Result := FType.Compare(FValue, TBox<T>(Obj).Peek);
end;

constructor TBox<T>.Create;
begin
  { Call upper constructor }
  Create(TType<T>.Default, default(T));
end;

constructor TBox<T>.Create(const AType: IType<T>; const AValue: T);
begin
  if AType = nil then
    ExceptionHelper.Throw_ArgumentNilError('AType');

  { Copy the value in }
  FValue := AValue;
  FIsBoxed := true;
  FType := AType;
end;

constructor TBox<T>.Create(const AType: IType<T>);
begin
  { Call upper constructor }
  Create(AType, default(T));
end;

procedure TBox<T>.Deserialize(const AData: TDeserializationData);
var
  LValueInfo: TValueInfo;
begin
  LValueInfo := TValueInfo.Create(SSerValue);

  { Restore the Type }
  FType := TType<T>.Default;

  { Get the "boxed" flag }
  AData.GetValue(SIsDefined, FIsBoxed);

  { Deserialize if required }
  if FIsBoxed then
    FType.Deserialize(SSerValue, FValue, AData);
end;

destructor TBox<T>.Destroy;
begin
  { If the value is still boxed in, clean it up on destruction }
  if (FIsBoxed) and (FType <> nil) and (FType.Management = tmManual) then
    FType.Cleanup(FValue);

  inherited;
end;

function TBox<T>.Equals(Value: T): Boolean;
begin
  { Verify that we actually have a boxed value }
  if not FIsBoxed then
    ExceptionHelper.Throw_TheBoxIsEmpty();

  { Use the provided type class }
  Result := FType.AreEqual(FValue, Value);
end;

function TBox<T>.Equals(Obj: TObject): Boolean;
begin
  if not (Obj is TBox<T>) then
    ExceptionHelper.Throw_ArgumentNotSameTypeError('Obj');

  { Verify that we actually have a boxed value }
  if not FIsBoxed then
    ExceptionHelper.Throw_TheBoxIsEmpty();

  { Use the provided type class }
  Result := FType.AreEqual(FValue, TBox<T>(Obj).Peek);
end;

function TBox<T>.GetHashCode: Integer;
begin
  { Verify that we actually have a boxed value }
  if not FIsBoxed then
    ExceptionHelper.Throw_TheBoxIsEmpty();

  { Use the provided type class }
  Result := FType.GenerateHashCode(FValue);
end;

function TBox<T>.HasBoxedValue: Boolean;
begin
  Result := FIsBoxed;
end;

function TBox<T>.Peek: T;
begin
  { Verify that we actually have a boxed value }
  if not FIsBoxed then
    ExceptionHelper.Throw_TheBoxIsEmpty();

  { Only peek, but do not unbox the value }
  Result := FValue;
end;

procedure TBox<T>.Serialize(const AData: TSerializationData);
begin
  { Only serialize the value if it is set }
  AData.AddValue(SIsDefined, FIsBoxed);

  { Write the value down }
  if FIsBoxed then
    FType.Serialize(SSerValue, FValue, AData)
  else
    FType.Serialize(SSerValue, default(T), AData);
end;

function TBox<T>.ToString: string;
begin
  { Verify that we actually have a boxed value }
  if not FIsBoxed then
    ExceptionHelper.Throw_TheBoxIsEmpty();

  { Use the provided type class }
  Result := FType.GetString(FValue);
end;

function TBox<T>.TryPeek(out AValue: T): Boolean;
begin
  { Return the value and verify that it is boxed }
  Result := FIsBoxed;

  if Result then
    AValue := FValue;
end;

function TBox<T>.TryUnbox(out AValue: T): Boolean;
begin
  { Peek the value}
  Result := TryPeek(AValue);

  { Mark as unboxed }
  FIsBoxed := false;
end;

function TBox<T>.Unbox: T;
begin
  { Return the value by peeking }
  Result := Peek();

  { Mark this instance as used }
  FIsBoxed := false;
end;

function TBox<T>.UnboxAndFree: T;
begin
  { Grab the value by unboxing it }
  Result := Unbox();

  { And free the instance }
  Destroy();
end;

{ TBoxType<T> }
function TBoxType<T>.Compare(const AValue1, AValue2: T): Integer;
begin
  { Use NILs for now }
  if (AValue1 = nil) and (AValue2 <> nil) then
    Exit(-1);

  if (AValue1 <> nil) and (AValue2 = nil) then
    Exit(1);

  if (AValue1 = nil) and (AValue2 = nil) then
    Exit(0);

  { Use normal comparing }
  Result := AValue1.CompareTo(AValue2);
end;

constructor TBoxType<T>.Create;
begin
  inherited Create(true);
end;

initialization
  { Register custom type }
  TType<TBox>.Register(TBoxType<TBox>);

finalization
  { Unregister custom type }
  TType<TBox>.Unregister();

end.
