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
unit DeHL.Collections.MultiMap;
interface
uses SysUtils,
     DeHL.Base,
     DeHL.Types,
     DeHL.Exceptions,
     DeHL.KeyValuePair,
     DeHL.Arrays,
     DeHL.Collections.Base,
     DeHL.Collections.Abstract,
     DeHL.Collections.List,
     DeHL.Collections.Dictionary;

type
  { Multi-Map based on a dictionary and a list }
  TMultiMap<TKey, TValue> = class(TAbstractMultiMap<TKey, TValue>)
  private
    FInitialCapacity: Cardinal;

  protected
    { Provide our implementations }
    function CreateDictionary(const AKeyType: IType<TKey>): IDictionary<TKey, IList<TValue>>; override;
    function CreateList(const AValueType: IType<TValue>): IList<TValue>; override;

  public
    { Local constructors }
    constructor Create(const InitialCapacity: Cardinal); overload;
    constructor Create(const AKeyType: IType<TKey>; const AValueType: IType<TValue>; const InitialCapacity: Cardinal); overload;
  end;

  { The object variant }
  TObjectMultiMap<TKey, TValue: class> = class sealed(TMultiMap<TKey, TValue>)
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

{ TMultiMap<TKey, TValue> }

constructor TMultiMap<TKey, TValue>.Create(const InitialCapacity: Cardinal);
begin
  FInitialCapacity := InitialCapacity;
  inherited Create();
end;

constructor TMultiMap<TKey, TValue>.Create(
  const AKeyType: IType<TKey>; const AValueType: IType<TValue>; const InitialCapacity: Cardinal);
begin
  FInitialCapacity := InitialCapacity;
  inherited Create(AKeyType, AValueType);
end;

function TMultiMap<TKey, TValue>.CreateDictionary(const AKeyType: IType<TKey>): IDictionary<TKey, IList<TValue>>;
var
  Cap: Cardinal;
begin
  { Create a simple dictionary }
  if FInitialCapacity = 0 then
    Cap := DefaultArrayLength
  else
    Cap := FInitialCapacity;

  Result := TDictionary<TKey, IList<TValue>>.Create(AKeyType, TType<IList<TValue>>.Default, Cap);
end;

function TMultiMap<TKey, TValue>.CreateList(const AValueType: IType<TValue>): IList<TValue>;
begin
  { Create a simple list }
  Result := TList<TValue>.Create(AValueType);
end;

{ TObjectMultiMap<TKey, TValue> }

procedure TObjectMultiMap<TKey, TValue>.InstallTypes(const AKeyType: IType<TKey>; const AValueType: IType<TValue>);
begin
  { Create a wrapper over the real type class and switch it }
  FKeyWrapperType := TObjectWrapperType<TKey>.Create(AKeyType);
  FValueWrapperType := TObjectWrapperType<TValue>.Create(AValueType);

  { Install overridden type }
  inherited InstallTypes(FKeyWrapperType, FValueWrapperType);
end;

function TObjectMultiMap<TKey, TValue>.GetOwnsKeys: Boolean;
begin
  Result := FKeyWrapperType.AllowCleanup;
end;

function TObjectMultiMap<TKey, TValue>.GetOwnsValues: Boolean;
begin
  Result := FValueWrapperType.AllowCleanup;
end;

procedure TObjectMultiMap<TKey, TValue>.SetOwnsKeys(const Value: Boolean);
begin
  FKeyWrapperType.AllowCleanup := Value;
end;

procedure TObjectMultiMap<TKey, TValue>.SetOwnsValues(const Value: Boolean);
begin
  FValueWrapperType.AllowCleanup := Value;
end;

end.
