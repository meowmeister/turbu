(*
* Copyright (c) 2008-2010, Ciobanu Alexandru
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
unit DeHL.Scoped;
interface
uses SysUtils,
     DeHL.Base,
     DeHL.Serialization,
     DeHL.Exceptions,
     DeHL.Types;

type
  { Scope pointer }
  Scoped<T: class> = record
  private
  type
    { Scope only class. It's a base for our auto-cleaner }
    TScopeOnly = class(TRefCountedObject, IInterface)
    private
      FClass : T;

    public
      destructor Destroy(); override;

    end;

  var
    FClass: T;
    FIntf: IInterface;

    function GetClass(): T;
{$HINTS OFF}
    class constructor Create();
    class destructor Destroy();
{$HINTS ON}
  public
    { Implicit operator }
    class operator Implicit(const AClass: T) : Scoped<T>;
    class operator Implicit(const AScopePtr: Scoped<T>) : T;

    { Value property }
    property Value: T read GetClass;
  end;

  { Nullable Support }
  TScopedType<T: class> = class(TRecordType<Scoped<T>>)
  private
    FType: IType<T>;

  protected
    { Serialization }
    procedure DoSerialize(const AInfo: TValueInfo; const AValue: Scoped<T>; const AContext: ISerializationContext); override;
    procedure DoDeserialize(const AInfo: TValueInfo; out AValue: Scoped<T>; const AContext: IDeserializationContext); override;

  public
    { Constructors }
    constructor Create(); overload; override;
    constructor Create(const AType: IType<T>); reintroduce; overload;

    { Comparator }
    function Compare(const AValue1, AValue2: Scoped<T>): Integer; override;

    { Equality Comparator }
    function AreEqual(const AValue1, AValue2: Scoped<T>): Boolean; override;

    { Hash code provider }
    function GenerateHashCode(const AValue: Scoped<T>): Integer; override;

    { Get String representation }
    function GetString(const AValue: Scoped<T>): String; override;

    { Variant Conversion }
    function TryConvertToVariant(const AValue: Scoped<T>; out ORes: Variant): Boolean; override;
    function TryConvertFromVariant(const AValue: Variant; out ORes: Scoped<T>): Boolean; override;
  end;

implementation

{ Scoped<T> }

class constructor Scoped<T>.Create;
begin
  { Register custom type }
  if not TType<Scoped<T>>.IsRegistered then
    TType<Scoped<T>>.Register(TScopedType<T>);
end;

class destructor Scoped<T>.Destroy;
begin
  { Unregister the custom type }
  if TType<Scoped<T>>.IsRegistered then
    TType<Scoped<T>>.Unregister();
end;

function Scoped<T>.GetClass() : T;
begin
  if FIntf = nil then
     Exit(nil)
  else
     Exit(FClass);
end;

class operator Scoped<T>.Implicit(const AScopePtr: Scoped<T>) : T;
begin
  Result := AScopePtr.Value;
end;

class operator Scoped<T>.Implicit(const AClass: T) : Scoped<T>;
var
  Scope : TScopeOnly;
begin
  { Check AClass }
  Scope := TScopeOnly.Create();
  Scope.FClass := AClass;

  Result.FIntf := Scope;
  Result.FClass := AClass;
end;

{ Scoped<T>.TScopeOnly }

destructor Scoped<T>.TScopeOnly.Destroy();
begin
  { Call closure function}
  FreeAndNil(FClass);

  inherited;
end;

{ TScopedType<T> }

function TScopedType<T>.AreEqual(const AValue1, AValue2: Scoped<T>): Boolean;
begin
  { Call the internal type }
  Result := FType.AreEqual(AValue1.Value, AValue2.Value);
end;

function TScopedType<T>.Compare(const AValue1, AValue2: Scoped<T>): Integer;
begin
  { Call the internal type }
  Result := FType.Compare(AValue1.Value, AValue2.Value);
end;

constructor TScopedType<T>.Create(const AType: IType<T>);
begin
  inherited Create();

  if AType = nil then
    ExceptionHelper.Throw_ArgumentNilError('AType');

  FType := AType;
end;

procedure TScopedType<T>.DoDeserialize(const AInfo: TValueInfo;
  out AValue: Scoped<T>; const AContext: IDeserializationContext);
begin
  { Unsupported by default }
  ExceptionHelper.Throw_Unserializable(AInfo.Name, Name);
end;

procedure TScopedType<T>.DoSerialize(const AInfo: TValueInfo; const AValue: Scoped<T>; const AContext: ISerializationContext);
begin
  { Unsupported by default }
  ExceptionHelper.Throw_Unserializable(AInfo.Name, Name);
end;

constructor TScopedType<T>.Create;
begin
  inherited;

  { Obtain the type }
  FType := TType<T>.Default;
end;

function TScopedType<T>.GenerateHashCode(const AValue: Scoped<T>): Integer;
begin
  { Call the internal type }
  Result := FType.GenerateHashCode(AValue.Value);
end;

function TScopedType<T>.GetString(const AValue: Scoped<T>): String;
begin
  { Call the internal type }
  Result := FType.GetString(AValue.Value);
end;

function TScopedType<T>.TryConvertFromVariant(const AValue: Variant; out ORes: Scoped<T>): Boolean;
var
  LV: T;
begin
  { Use the enclosed type }
  Result := FType.TryConvertFromVariant(AValue, LV);

  if Result then
    ORes := LV;
end;

function TScopedType<T>.TryConvertToVariant(const AValue: Scoped<T>; out ORes: Variant): Boolean;
begin
  { Use the enclosed type }
  Result := FType.TryConvertToVariant(AValue.Value, ORes);
end;

end.
