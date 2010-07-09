(*
* Copyright (c) 2009, Bentea Lucian
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
unit DeHL.Collections.HashTree;
interface
uses SysUtils,
     DeHL.Base,
     DeHL.Types,
     DeHL.Exceptions,
     DeHL.Arrays,
     DeHL.KeyValuePair,
     DeHL.Collections.Base,
     DeHL.Collections.Dictionary;

type
  { Generic Dictionary }
  THashTree<TKey, TValue> = class(TEnexAssociativeCollection<TKey, TValue>)
  private
  type
    { Generic Hash Tree Pairs Enumerator }
    TPairEnumerator = class(TEnumerator<TKeyValuePair<TKey,TValue>>)
    private
      FEnumerator  : IEnumerator<TKey>;
      FHashTree    : THashTree<TKey, TValue>;

    public
      { Constructor }
      constructor Create(const AHashTree: THashTree<TKey, TValue>);

      { Destructor }
      destructor Destroy(); override;

      function GetCurrent(): TKeyValuePair<TKey, TValue>; override;
      function MoveNext(): Boolean; override;
    end;

    { Generic Hash Tree Values Enumerator }
    TValueEnumerator = class(TEnumerator<TValue>)
    private
      FEnumerator  : IEnumerator<THashTree<TKey, TValue>>;
      FHashTree    : THashTree<TKey, TValue>;

    public
      { Constructor }
      constructor Create(const AHashTree: THashTree<TKey, TValue>);

      { Destructor }
      destructor Destroy(); override;

      function GetCurrent(): TValue; override;
      function MoveNext(): Boolean; override;
    end;

    { Generic Hash Tree Values Collection }
    TValueCollection = class(TEnexCollection<TValue>)
    private
      FHashTree: THashTree<TKey, TValue>;

    protected

      { Hidden }
      function GetCount: Cardinal; override;

    public
      { Constructor }
      constructor Create(const AHashTree: THashTree<TKey, TValue>);

      { Destructor }
      destructor Destroy(); override;

      { Property }
      property Count: Cardinal read GetCount;

      { IEnumerable/ ICollection support }
      function GetEnumerator(): IEnumerator<TValue>; override;

      { Copy-To }
      procedure CopyTo(var AArray: array of TValue); overload; override;
      procedure CopyTo(var AArray: array of TValue; const StartIndex: Cardinal); overload; override;

      { Enex Overrides }
      function Empty(): Boolean; override;
    end;

    var
      FValue          : TValue;
      FChildren       : TDictionary<TKey, THashTree<TKey, TValue>>;
      FParent         : THashTree<TKey, TValue>;
      FValueCollection: IEnexCollection<TValue>;
      FKeyCollection  : IEnexCollection<TKey>;

    function GetChildByKey(const AKey: TKey): THashTree<TKey, TValue>;
    procedure CalcAggregatedCount(const ANode: THashTree<TKey, TValue>; var AggCount: Cardinal);

    {
      internal routine that copies the entire tree rooted at the current instance
      into the given array, using Breadth-First Search
    }
    procedure CopyToArray(var AArray: array of TKeyValuePair<TKey,TValue>; var Index: Integer; const ANode: THashTree<TKey, TValue>);

  protected
    { Hidden }
    function GetCount: Cardinal; override;
    function GetAggregatedCount: Cardinal;

  public
    { Constructors }
    constructor Create(); overload;
    constructor Create(const InitialCapacity: Cardinal); overload;
    constructor Create(const AEnumerable: IEnumerable<TKeyValuePair<TKey,TValue>>); overload;
    constructor Create(const AArray: array of TKeyValuePair<TKey,TValue>); overload;
    constructor Create(const AArray: TDynamicArray<TKeyValuePair<TKey, TValue>>); overload;
    constructor Create(const AArray: TFixedArray<TKeyValuePair<TKey, TValue>>); overload;

    constructor Create(const AKeyType: IType<TKey>; const AValueType: IType<TValue>); overload;
    constructor Create(const AKeyType: IType<TKey>; const AValueType: IType<TValue>; const InitialCapacity: Cardinal); overload;
    constructor Create(const AKeyType: IType<TKey>; const AValueType: IType<TValue>;
          const AEnumerable: IEnumerable<TKeyValuePair<TKey,TValue>>); overload;
    constructor Create(const AKeyType: IType<TKey>; const AValueType: IType<TValue>;
          const AArray: array of TKeyValuePair<TKey,TValue>); overload;
    constructor Create(const AKeyType: IType<TKey>; const AValueType: IType<TValue>;
          const AArray: TDynamicArray<TKeyValuePair<TKey,TValue>>); overload;
    constructor Create(const AKeyType: IType<TKey>; const AValueType: IType<TValue>;
          const AArray: TFixedArray<TKeyValuePair<TKey,TValue>>); overload;

    { Destructor }
    destructor Destroy(); override;

    { Add a child to the current instance }
    procedure Add(const APair: TKeyValuePair<TKey,TValue>); overload;
    procedure Add(const AKey: TKey; const AValue: TValue); overload;

    { Remove the child node with the given key }
    procedure Remove(const AKey: TKey); overload;

    { Clears all the children instances and sets the value of the current instance to the default one }
    procedure Clear(); overload;

    { Lookup }
    function ContainsKey(const AKey: TKey): Boolean;
    function ContainsValue(const AValue: TValue): Boolean;
    function TryGetValue(const AKey: TKey; out FoundValue: TValue): Boolean;

    { Copy-To }
    procedure ChildrenCopyTo(var AArray: array of TKeyValuePair<TKey,TValue>); overload;
    procedure ChildrenCopyTo(var AArray: array of TKeyValuePair<TKey,TValue>; const StartIndex: Cardinal); overload;

    { IEnumerable/ ICollection support }
    function GetEnumerator(): IEnumerator<TKeyValuePair<TKey,TValue>>; override;

    { The value of the current instance }
    property Value: TValue read FValue write FValue;

    { The children of the current instance, indexed by their keys }
    property Children[const Key: TKey]: THashTree<TKey, TValue> read GetChildByKey; default;

    { The number of children of the current instance }
    property Count: Cardinal read GetCount;
    property AggregatedCount: Cardinal read GetAggregatedCount;

    property Keys: IEnexCollection<TKey> read FKeyCollection;
    property Values: IEnexCollection<TValue> read FValueCollection;

    { Copies the entire tree rooted at the current instance into the given array, using Breadth-First Search }
    procedure CopyTo(var AArray: array of TKeyValuePair<TKey,TValue>); overload; override;
    procedure CopyTo(var AArray: array of TKeyValuePair<TKey,TValue>; const StartIndex: Cardinal); overload; override;

    { Enex - selectors }
    function SelectKeys(): IEnexCollection<TKey>; override;
    function SelectValues(): IEnexCollection<TValue>; override;
  end;

  { The object variant }
  TObjectHashTree<TKey, TValue: class> = class sealed(THashTree<TKey, TValue>)
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

{ THashTree<TKey, TValue> }

const
  DefaultNoChildren = 32;

procedure THashTree<TKey, TValue>.Add(
  const APair: TKeyValuePair<TKey, TValue>);
begin
  Add(APair.Key, APair.Value);
end;

procedure THashTree<TKey, TValue>.Add(const AKey: TKey;
  const AValue: TValue);
begin
  FChildren.Add(AKey, THashTree<TKey, TValue>.Create(FKeyType, FValueType));
  FChildren[AKey].FValue := AValue;
  FChildren[AKey].FParent := Self;
end;

procedure THashTree<TKey, TValue>.Clear;
begin
  { Clear children }
  FChildren.Clear;

  { Clear the value if required }
  if (FValueType <> nil) and (FValueType.Management = tmManual) then
    FValueType.Cleanup(FValue);
end;

function THashTree<TKey, TValue>.ContainsKey(const AKey: TKey): Boolean;
begin
  Result := FChildren.ContainsKey(AKey);
end;

function THashTree<TKey, TValue>.ContainsValue(const AValue: TValue): Boolean;
var
  I: TKeyValuePair<TKey, THashTree<TKey, TValue>>;
begin
  Result := False;
  for I in FChildren do
    if FValueType.AreEqual(I.Value.FValue, AValue) then
    begin
      Result := True;
      exit;
    end;
end;

procedure THashTree<TKey, TValue>.CopyTo(
  var AArray: array of TKeyValuePair<TKey, TValue>);
begin
  CopyTo(AArray, 0);
end;

procedure THashTree<TKey, TValue>.CopyTo(
  var AArray: array of TKeyValuePair<TKey, TValue>; const StartIndex: Cardinal);
var
  Index: Integer;
begin
  { Check for indexes }
  if StartIndex >= Cardinal(Length(AArray)) then
    ExceptionHelper.Throw_ArgumentOutOfRangeError('StartIndex');

  if (Cardinal(Length(AArray)) - StartIndex) < GetAggregatedCount - 1 then
     ExceptionHelper.Throw_ArgumentOutOfSpaceError('AArray');

  Index := StartIndex;
  CopyToArray(AArray, Index, Self);
end;

procedure THashTree<TKey, TValue>.CopyToArray(
  var AArray: array of TKeyValuePair<TKey, TValue>; var Index: Integer;
  const ANode: THashTree<TKey, TValue>);
var
  I: TKeyValuePair<TKey, THashTree<TKey, TValue>>;
  no: Cardinal;
  val: THashTree<TKey, TValue>;
begin
  for I in ANode.FChildren do
  begin
    AArray[Index] := TKeyValuePair<TKey, TValue>.Create(I.Key, I.Value.FValue);
    Inc(Index);
  end;
  for I in ANode.FChildren do
    if I.Value.Count > 0 then
      CopyToArray(AArray, Index, I.Value);
end;

procedure THashTree<TKey, TValue>.ChildrenCopyTo(
  var AArray: array of TKeyValuePair<TKey, TValue>);
begin
  ChildrenCopyTo(AArray, 0);
end;

procedure THashTree<TKey, TValue>.CalcAggregatedCount(
  const ANode: THashTree<TKey, TValue>; var AggCount: Cardinal);
var
  I: TKeyValuePair<TKey, THashTree<TKey, TValue>>;
begin
  for I in ANode.FChildren do
  begin
    CalcAggregatedCount(I.Value, AggCount);
    Inc(AggCount);
  end;
end;

procedure THashTree<TKey, TValue>.ChildrenCopyTo(
  var AArray: array of TKeyValuePair<TKey, TValue>; const StartIndex: Cardinal);
var
  I: TKeyValuePair<TKey, THashTree<TKey, TValue>>;
  X: Integer;
begin
  { Check for indexes }
  if (Cardinal(Length(AArray)) - StartIndex) < FChildren.Count then
     ExceptionHelper.Throw_ArgumentOutOfSpaceError('AArray');

  X := StartIndex;

  for I in FChildren do
  begin
    AArray[X] := TKeyValuePair<TKey, TValue>.Create(I.Key, I.Value.FValue);
    Inc(X);
  end;
end;

destructor THashTree<TKey, TValue>.Destroy;
begin
  { Clear this node and kill classes }
  if FChildren <> nil then
    Clear();

  FChildren.Free;

  inherited;
end;

function THashTree<TKey, TValue>.GetAggregatedCount: Cardinal;
begin
  Result := 1;
  CalcAggregatedCount(Self, Result);
end;

function THashTree<TKey, TValue>.GetChildByKey(
  const AKey: TKey): THashTree<TKey, TValue>;
begin
  if not FChildren.ContainsKey(AKey) then
    Add(AKey, default(TValue));
  Result := FChildren.Items[AKey];
end;

function THashTree<TKey, TValue>.GetCount: Cardinal;
begin
  Result := FChildren.Count;
end;

function THashTree<TKey, TValue>.GetEnumerator: IEnumerator<TKeyValuePair<TKey, TValue>>;
begin
  Result := THashTree<TKey, TValue>.TPairEnumerator.Create(Self);
end;

procedure THashTree<TKey, TValue>.Remove(const AKey: TKey);
begin
  FChildren.Remove(AKey);
end;

function THashTree<TKey, TValue>.SelectKeys: IEnexCollection<TKey>;
begin
  Result := Keys;
end;

function THashTree<TKey, TValue>.SelectValues: IEnexCollection<TValue>;
begin
  Result := Values;
end;

function THashTree<TKey, TValue>.TryGetValue(const AKey: TKey;
  out FoundValue: TValue): Boolean;
var
  I: TKeyValuePair<TKey, THashTree<TKey, TValue>>;
begin
  Result := False;
  for I in FChildren do
    if FKeyType.AreEqual(I.Key, AKey) then
    begin
      Result := True;
      FoundValue := I.Value.FValue;
      exit;
    end;
end;

constructor THashTree<TKey, TValue>.Create(
  const AArray: TFixedArray<TKeyValuePair<TKey, TValue>>);
begin
  Create(TType<TKey>.Default, TType<TValue>.Default, AArray);
end;

constructor THashTree<TKey, TValue>.Create(
  const AArray: TDynamicArray<TKeyValuePair<TKey, TValue>>);
begin
  Create(TType<TKey>.Default, TType<TValue>.Default, AArray);
end;

constructor THashTree<TKey, TValue>.Create(const AKeyType: IType<TKey>;
  const AValueType: IType<TValue>;
  const AArray: TFixedArray<TKeyValuePair<TKey, TValue>>);
var
  I: Cardinal;
begin
  { Call upper constructor }
  Create(AKeyType, AValueType, DefaultNoChildren);

  { Copy all items in }
  if AArray.Length > 0 then
    for I := 0 to AArray.Length - 1 do
    begin
{$IFNDEF BUG_GENERIC_INCOMPAT_TYPES}
      Add(AArray[I]);
{$ELSE}
      Add(AArray[I].Key, AArray[I].Value);
{$ENDIF}
    end;
end;

constructor THashTree<TKey, TValue>.Create(const AKeyType: IType<TKey>;
  const AValueType: IType<TValue>;
  const AArray: TDynamicArray<TKeyValuePair<TKey, TValue>>);
var
  I: Cardinal;
begin
  { Call upper constructor }
  Create(AKeyType, AValueType, DefaultNoChildren);

  { Copy all items in }
  if AArray.Length > 0 then
    for I := 0 to AArray.Length - 1 do
    begin
{$IFNDEF BUG_GENERIC_INCOMPAT_TYPES}
      Add(AArray[I]);
{$ELSE}
      Add(AArray[I].Key, AArray[I].Value);
{$ENDIF}
    end;
end;

{ THashTree<TKey, TValue>.TValueEnumerator }

constructor THashTree<TKey, TValue>.TValueEnumerator.Create(
  const AHashTree: THashTree<TKey, TValue>);
begin
  { Initialize}
  FHashTree := AHashTree;
  KeepObjectAlive(FHashTree);

  FEnumerator := AHashTree.FChildren.Values.GetEnumerator;
end;

destructor THashTree<TKey, TValue>.TValueEnumerator.Destroy;
begin
  ReleaseObject(FHashTree);
  inherited;
end;

function THashTree<TKey, TValue>.TValueEnumerator.GetCurrent: TValue;
begin
  Result := FEnumerator.GetCurrent.Value;
end;

function THashTree<TKey, TValue>.TValueEnumerator.MoveNext: Boolean;
begin
  Result := FEnumerator.MoveNext;
end;

{ THashTree<TKey, TValue>.TValueCollection }

procedure THashTree<TKey, TValue>.TValueCollection.CopyTo(
  var AArray: array of TValue; const StartIndex: Cardinal);
var
  I: TValueEnumerator;
  X: Cardinal;
begin
  { Check for indexes }
  if StartIndex >= Cardinal(Length(AArray)) then
    ExceptionHelper.Throw_ArgumentOutOfRangeError('StartIndex');

  if (Cardinal(Length(AArray)) - StartIndex) < FHashTree.Count then
     ExceptionHelper.Throw_ArgumentOutOfSpaceError('AArray');

  I := TValueEnumerator.Create(FHashTree);
  X := StartIndex;
  while I.MoveNext do
  begin
    AArray[X] := I.GetCurrent;
    Inc(X);
  end;
  I.Free;
end;

procedure THashTree<TKey, TValue>.TValueCollection.CopyTo(
  var AArray: array of TValue);
begin
  { call the more generic procedure }
  CopyTo(AArray, 0);
end;

constructor THashTree<TKey, TValue>.TValueCollection.Create(
  const AHashTree: THashTree<TKey, TValue>);
begin
  { Initialize}
  FHashTree := AHashTree;

  InstallType(FHashTree.ValueType);
end;

destructor THashTree<TKey, TValue>.TValueCollection.Destroy;
begin
  inherited;
end;

function THashTree<TKey, TValue>.TValueCollection.Empty: Boolean;
begin
  Result := (FHashTree.Count = 0);
end;

function THashTree<TKey, TValue>.TValueCollection.GetCount: Cardinal;
begin
  {
    number of elements is the same as the number of children
    of the associated hash tree instance
  }
  Result := FHashTree.Count;
end;

function THashTree<TKey, TValue>.TValueCollection.GetEnumerator: IEnumerator<TValue>;
begin
  Result := TValueEnumerator.Create(FHashTree);
end;

{ THashTree<TKey, TValue>.TPairEnumerator }

constructor THashTree<TKey, TValue>.TPairEnumerator.Create(
  const AHashTree: THashTree<TKey, TValue>);
begin
  FHashTree := AHashTree;
  KeepObjectAlive(FHashTree);

  FEnumerator := FHashTree.Keys.GetEnumerator;
end;

destructor THashTree<TKey, TValue>.TPairEnumerator.Destroy;
begin
  ReleaseObject(FHashTree);
  inherited;
end;

function THashTree<TKey, TValue>.TPairEnumerator.GetCurrent: TKeyValuePair<TKey, TValue>;
begin
  Result := TKeyValuePair<TKey, TValue>.Create(FEnumerator.Current , FHashTree.Children[FEnumerator.Current].Value);
end;

function THashTree<TKey, TValue>.TPairEnumerator.MoveNext: Boolean;
begin
  Result := FEnumerator.MoveNext;
end;

constructor THashTree<TKey, TValue>.Create;
begin
  Create(TType<TKey>.Default, TType<TValue>.Default);
end;

constructor THashTree<TKey, TValue>.Create(const InitialCapacity: Cardinal);
begin
  Create(TType<TKey>.Default, TType<TValue>.Default, InitialCapacity);
end;

constructor THashTree<TKey, TValue>.Create(
  const AEnumerable: IEnumerable<TKeyValuePair<TKey, TValue>>);
begin
  Create(TType<TKey>.Default, TType<TValue>.Default, AEnumerable);
end;

constructor THashTree<TKey, TValue>.Create(
  const AKeyType: IType<TKey>;
  const AValueType: IType<TValue>; const InitialCapacity: Cardinal);
begin
  { Initialize instance }
  if (AKeyType = nil) then
     ExceptionHelper.Throw_ArgumentNilError('AKeyType');

  if (AValueType = nil) then
     ExceptionHelper.Throw_ArgumentNilError('AValueType');

  { Install types }
  InstallTypes(AKeyType, AValueType);

  FChildren := TDictionary<TKey, THashTree<TKey, TValue>>.Create(
    FKeyType, TClassType<THashTree<TKey, TValue>>.Create(True)
  );

  FKeyCollection := FChildren.Keys;
  FValueCollection := TValueCollection.Create(Self);

  FParent := nil;

  { Call the default (Thread Safety) constructor }
  inherited Create();
end;

constructor THashTree<TKey, TValue>.Create(const AKeyType: IType<TKey>;
  const AValueType: IType<TValue>;
  const AEnumerable: IEnumerable<TKeyValuePair<TKey, TValue>>);
var
  V: TKeyValuePair<TKey, TValue>;
begin
  { Call upper constructor }
  Create(AKeyType, AValueType, DefaultNoChildren);

  if (AEnumerable = nil) then
     ExceptionHelper.Throw_ArgumentNilError('AEnumerable');

  { Pump in all items }
  for V in AEnumerable do
  begin
    Add(V.Key, V.Value);
  end;
end;

constructor THashTree<TKey, TValue>.Create(
  const AKeyType: IType<TKey>;
  const AValueType: IType<TValue>);
begin
  { Call upper constructor }
  Create(AKeyType, AValueType, DefaultNoChildren);
end;

constructor THashTree<TKey, TValue>.Create(
  const AArray: array of TKeyValuePair<TKey, TValue>);
begin
  Create(TType<TKey>.Default, TType<TValue>.Default, AArray);
end;

constructor THashTree<TKey, TValue>.Create(
  const AKeyType: IType<TKey>;
  const AValueType: IType<TValue>;
  const AArray: array of TKeyValuePair<TKey, TValue>);
var
  I: Integer;
begin
  { Call upper constructor }
  Create(AKeyType, AValueType, DefaultNoChildren);

  { Copy all items in }
  for I := 0 to Length(AArray) - 1 do
  begin
    Add(AArray[I]);
  end;
end;

{ TObjectHashTree<TKey, TValue> }

procedure TObjectHashTree<TKey, TValue>.InstallTypes(const AKeyType: IType<TKey>; const AValueType: IType<TValue>);
begin
  { Create a wrapper over the real type class and switch it }
  FKeyWrapperType := TObjectWrapperType<TKey>.Create(AKeyType);
  FValueWrapperType := TObjectWrapperType<TValue>.Create(AValueType);

  { Install overridden type }
  inherited InstallTypes(FKeyWrapperType, FValueWrapperType);
end;

function TObjectHashTree<TKey, TValue>.GetOwnsKeys: Boolean;
begin
  Result := FKeyWrapperType.AllowCleanup;
end;

function TObjectHashTree<TKey, TValue>.GetOwnsValues: Boolean;
begin
  Result := FValueWrapperType.AllowCleanup;
end;

procedure TObjectHashTree<TKey, TValue>.SetOwnsKeys(const Value: Boolean);
begin
  FKeyWrapperType.AllowCleanup := Value;
end;

procedure TObjectHashTree<TKey, TValue>.SetOwnsValues(const Value: Boolean);
begin
  FValueWrapperType.AllowCleanup := Value;
end;

end.
