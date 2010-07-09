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
unit DeHL.Collections.SortedBag;
interface
uses SysUtils,
     DeHL.Base,
     DeHL.Types,
     DeHL.StrConsts,
     DeHL.Exceptions,
     DeHL.KeyValuePair,
     DeHL.Arrays,
     DeHL.Serialization,
     DeHL.Collections.Base,
     DeHL.Collections.Abstract,
     DeHL.Collections.SortedDictionary;

type
  { Generic Dictionary }
  TSortedBag<T> = class(TAbstractBag<T>)
  private
    FAscSort: Boolean;

  protected
    { Provide our implementations }
    function CreateDictionary(const AType: IType<T>): IDictionary<T, Cardinal>; override;

    { Serialization overrides }
    procedure StartSerializing(const AData: TSerializationData); override;
    procedure StartDeserializing(const AData: TDeserializationData); override;
    procedure DeserializeElement(const AElement: T); override;
  public
    { Constructors }
    constructor Create(const Ascending: Boolean = true); overload;
    constructor Create(const AEnumerable: IEnumerable<T>; const Ascending: Boolean = true); overload;
    constructor Create(const AArray: array of T; const Ascending: Boolean = true); overload;
    constructor Create(const AArray: TDynamicArray<T>; const Ascending: Boolean = true); overload;
    constructor Create(const AArray: TFixedArray<T>; const Ascending: Boolean = true); overload;

    constructor Create(const AType: IType<T>; const Ascending: Boolean = true); overload;
    constructor Create(const AType: IType<T>; const AEnumerable: IEnumerable<T>; const Ascending: Boolean = true); overload;
    constructor Create(const AType: IType<T>; const AArray: array of T; const Ascending: Boolean = true); overload;
    constructor Create(const AType: IType<T>; const AArray: TDynamicArray<T>; const Ascending: Boolean = true); overload;
    constructor Create(const AType: IType<T>; const AArray: TFixedArray<T>; const Ascending: Boolean = true); overload;
  end;

  { The object variant }
  TObjectSortedBag<T: class> = class sealed(TSortedBag<T>)
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

{ TSortedBag<T> }

constructor TSortedBag<T>.Create(const AArray: TFixedArray<T>; const Ascending: Boolean);
begin
  { Call upper constructor }
  FAscSort := Ascending;
  inherited Create(AArray);
end;

constructor TSortedBag<T>.Create(const AArray: TDynamicArray<T>; const Ascending: Boolean);
begin
  { Call upper constructor }
  FAscSort := Ascending;
  inherited Create(AArray);
end;

constructor TSortedBag<T>.Create(const AType: IType<T>; const AArray: TFixedArray<T>; const Ascending: Boolean);
begin
  { Call upper constructor }
  FAscSort := Ascending;
  inherited Create(AType, AArray);
end;

function TSortedBag<T>.CreateDictionary(const AType: IType<T>): IDictionary<T, Cardinal>;
begin
  { Create a sorted dictionary }
  Result := TSortedDictionary<T, Cardinal>.Create(AType, TType<Cardinal>.Default, FAscSort);
end;

procedure TSortedBag<T>.DeserializeElement(const AElement: T);
begin
  { Simple as hell ... }
  Add(AElement);
end;

procedure TSortedBag<T>.StartDeserializing(const AData: TDeserializationData);
var
  LAsc: Boolean;
begin
  AData.GetValue(SSerAscendingKeys, LAsc);

  { Call the constructor in this instance to initialize myself first }
  Create(LAsc);
end;

procedure TSortedBag<T>.StartSerializing(const AData: TSerializationData);
begin
  { Write the ascending sign }
  AData.AddValue(SSerAscendingKeys, FAscSort);
end;

constructor TSortedBag<T>.Create(const AType: IType<T>; const AArray: TDynamicArray<T>; const Ascending: Boolean);
begin
  { Call upper constructor }
  FAscSort := Ascending;
  inherited Create(AType, AArray);
end;

constructor TSortedBag<T>.Create(const Ascending: Boolean);
begin
  { Call upper constructor }
  FAscSort := Ascending;
  inherited Create();
end;

constructor TSortedBag<T>.Create(const AEnumerable: IEnumerable<T>; const Ascending: Boolean);
begin
  { Call upper constructor }
  FAscSort := Ascending;
  inherited Create(AEnumerable);
end;

constructor TSortedBag<T>.Create(const AType: IType<T>; const AEnumerable: IEnumerable<T>; const Ascending: Boolean);
begin
  { Call upper constructor }
  FAscSort := Ascending;
  inherited Create(AType, AEnumerable);
end;

constructor TSortedBag<T>.Create(const AType: IType<T>; const AArray: array of T; const Ascending: Boolean);
begin
  { Call upper constructor }
  FAscSort := Ascending;
  inherited Create(AType, AArray);
end;

constructor TSortedBag<T>.Create(const AType: IType<T>; const Ascending: Boolean);
begin
  { Call upper constructor }
  FAscSort := Ascending;
  inherited Create(AType);
end;

constructor TSortedBag<T>.Create(const AArray: array of T; const Ascending: Boolean);
begin
  { Call upper constructor }
  FAscSort := Ascending;
  inherited Create(AArray);
end;

{ TObjectSortedBag<T> }

procedure TObjectSortedBag<T>.InstallType(const AType: IType<T>);
begin
  { Create a wrapper over the real type class and switch it }
  FWrapperType := TObjectWrapperType<T>.Create(AType);

  { Install overridden type }
  inherited InstallType(FWrapperType);
end;

function TObjectSortedBag<T>.GetOwnsObjects: Boolean;
begin
  Result := FWrapperType.AllowCleanup;
end;

procedure TObjectSortedBag<T>.SetOwnsObjects(const Value: Boolean);
begin
  FWrapperType.AllowCleanup := Value;
end;

end.
