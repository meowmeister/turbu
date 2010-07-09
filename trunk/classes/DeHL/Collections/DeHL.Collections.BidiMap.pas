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
unit DeHL.Collections.BidiMap;
interface
uses
  SysUtils,
  DeHL.Base,
  DeHL.Exceptions,
  DeHL.Types,
  DeHL.Arrays,
  DeHL.Serialization,
  DeHl.KeyValuePair,
  DeHL.Collections.Base,
  DeHL.Collections.Abstract,
  DeHL.Collections.DistinctMultiMap;


type
  { Multi-Map based on a dictionary and a list }
  TBidiMap<TKey, TValue> = class(TAbstractBidiMap<TKey, TValue>)
  private
    FInitialCapacity: Cardinal;

  protected
    { Override to provide map implemenatations }
    function CreateKeyMap(const AKeyType: IType<TKey>;
      const AValueType: IType<TValue>): IDistinctMultiMap<TKey, TValue>; override;

    function CreateValueMap(const AValueType: IType<TValue>;
      const AKeyType: IType<TKey>): IDistinctMultiMap<TValue, TKey>; override;

    { Serialization overrides }
    procedure StartSerializing(const AData: TSerializationData); override;
    procedure StartDeserializing(const AData: TDeserializationData); override;
    procedure DeserializePair(const AKey: TKey; const AValue: TValue); override;
  public
    { Local constructors }
    constructor Create(const InitialCapacity: Cardinal); overload;
    constructor Create(const AKeyType: IType<TKey>; const AValueType: IType<TValue>; const InitialCapacity: Cardinal); overload;
  end;

  { The object variant }
  TObjectBidiMap<TKey, TValue: class> = class sealed(TBidiMap<TKey, TValue>)
  private
    FKeyWrapperType: TObjectWrapperType<TKey>;
    FValueWrapperType: TObjectWrapperType<TValue>;

    { Getters/Setters for OwnsKeys }
    function GetOwnsKeys: Boolean;
    procedure SetOwnsKeys(const Value: Boolean);

    { Getters/Setters for OwnsValues }
    function GetOwnsValues: Boolean;
    procedure SetOwnsValues(const Value: Boolean);

  protected
    { Override in descendants to support proper stuff }
    procedure InstallTypes(const AKeyType: IType<TKey>; const AValueType: IType<TValue>); override;

  public
    { Object owning }
    property OwnsKeys: Boolean read GetOwnsKeys write SetOwnsKeys;
    property OwnsValues: Boolean read GetOwnsValues write SetOwnsValues;
  end;

implementation

const
  DefaultArrayLength = 32;

{ TBidiMap<TKey, TValue> }

constructor TBidiMap<TKey, TValue>.Create(const InitialCapacity: Cardinal);
begin
  FInitialCapacity := InitialCapacity;

  inherited Create();
end;

constructor TBidiMap<TKey, TValue>.Create(const AKeyType: IType<TKey>; const AValueType: IType<TValue>; const InitialCapacity: Cardinal);
begin
  FInitialCapacity := InitialCapacity;

  inherited Create(AKeyType, AValueType);
end;

function TBidiMap<TKey, TValue>.CreateKeyMap(const AKeyType: IType<TKey>;
  const AValueType: IType<TValue>): IDistinctMultiMap<TKey, TValue>;
var
  Cap: Cardinal;
begin
  { Create a simple dictionary }
  if FInitialCapacity = 0 then
    Cap := DefaultArrayLength
  else
    Cap := FInitialCapacity;

  { Use a simple non-sorted map }
  Result := TDistinctMultiMap<TKey, TValue>.Create(AKeyType, AValueType, Cap);
end;

function TBidiMap<TKey, TValue>.CreateValueMap(const AValueType: IType<TValue>;
  const AKeyType: IType<TKey>): IDistinctMultiMap<TValue, TKey>;
var
  Cap: Cardinal;
begin
  { Create a simple dictionary }
  if FInitialCapacity = 0 then
    Cap := DefaultArrayLength
  else
    Cap := FInitialCapacity;

  { Use a simple non-sorted map }
  Result := TDistinctMultiMap<TValue, TKey>.Create(AValueType, AKeyType, Cap);
end;

procedure TBidiMap<TKey, TValue>.DeserializePair(const AKey: TKey; const AValue: TValue);
begin
  { Very simple }
  Add(AKey, AValue);
end;

procedure TBidiMap<TKey, TValue>.StartDeserializing(const AData: TDeserializationData);
begin
  { Call the constructor in this instance to initialize myself first }
  Create();
end;

procedure TBidiMap<TKey, TValue>.StartSerializing(const AData: TSerializationData);
begin
  // Do nothing, just say that I am here and I can be serialized
end;

{ TObjectBidiMap<TKey, TValue> }

procedure TObjectBidiMap<TKey, TValue>.InstallTypes(const AKeyType: IType<TKey>; const AValueType: IType<TValue>);
begin
  { Create a wrapper over the real type class and switch it }
  FKeyWrapperType := TObjectWrapperType<TKey>.Create(AKeyType);
  FValueWrapperType := TObjectWrapperType<TValue>.Create(AValueType);

  { Install overridden type }
  inherited InstallTypes(FKeyWrapperType, FValueWrapperType);
end;

function TObjectBidiMap<TKey, TValue>.GetOwnsKeys: Boolean;
begin
  Result := FKeyWrapperType.AllowCleanup;
end;

function TObjectBidiMap<TKey, TValue>.GetOwnsValues: Boolean;
begin
  Result := FValueWrapperType.AllowCleanup;
end;

procedure TObjectBidiMap<TKey, TValue>.SetOwnsKeys(const Value: Boolean);
begin
  FKeyWrapperType.AllowCleanup := Value;
end;

procedure TObjectBidiMap<TKey, TValue>.SetOwnsValues(const Value: Boolean);
begin
  FValueWrapperType.AllowCleanup := Value;
end;

end.
