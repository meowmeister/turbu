(*
* Copyright (c) 2009, Ciobanu Alexandru
* All rights reserved.
*
* Redistribution and use in source and binary forms, wior without
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
unit DeHL.Collections.Abstract;
interface
uses SysUtils,
     DeHL.Base,
     DeHL.Types,
     DeHL.Exceptions,
     DeHL.KeyValuePair,
     DeHL.Arrays,
     DeHL.Serialization,     
     DeHL.Collections.Base;

type
  { Multi-Map base class }
  TAbstractMultiMap<TKey, TValue> = class abstract(TEnexAssociativeCollection<TKey, TValue>, IMultiMap<TKey, TValue>)
  private
  type
    { Generic MultiMap Pairs Enumerator }
    TPairEnumerator = class(TEnumerator<TKeyValuePair<TKey,TValue>>)
    private
      FVer         : Cardinal;
      FDict        : TAbstractMultiMap<TKey, TValue>;
      FValue       : TKeyValuePair<TKey, TValue>;

      FListIndex   : Cardinal;
      FDictEnum    : IEnumerator<TKeyValuePair<TKey, IList<TValue>>>;
      FList        : IList<TValue>;

    public
      { Constructor }
      constructor Create(const ADict: TAbstractMultiMap<TKey, TValue>);

      { Destructor }
      destructor Destroy(); override;

      function GetCurrent(): TKeyValuePair<TKey,TValue>; override;
      function MoveNext(): Boolean; override;
    end;

    { Generic MultiMap Keys Enumerator }
    TKeyEnumerator = class(TEnumerator<TKey>)
    private
      FVer         : Cardinal;
      FDict        : TAbstractMultiMap<TKey, TValue>;
      FValue       : TKey;
      FDictEnum    : IEnumerator<TKey>;

    public
      { Constructor }
      constructor Create(const ADict: TAbstractMultiMap<TKey, TValue>);

      { Destructor }
      destructor Destroy(); override;

      function GetCurrent(): TKey; override;
      function MoveNext(): Boolean; override;
    end;

    { Generic MultiMap Values Enumerator }
    TValueEnumerator = class(TEnumerator<TValue>)
    private
      FVer         : Cardinal;
      FDict        : TAbstractMultiMap<TKey, TValue>;
      FValue       : TValue;

      FListIndex   : Cardinal;
      FDictEnum    : IEnumerator<IList<TValue>>;
      FList        : IList<TValue>;

    public
      { Constructor }
      constructor Create(const ADict: TAbstractMultiMap<TKey, TValue>);

      { Destructor }
      destructor Destroy(); override;

      function GetCurrent(): TValue; override;
      function MoveNext(): Boolean; override;
    end;

    { Generic MultiMap Keys Collection }
    TKeyCollection = class(TEnexCollection<TKey>)
    private
      FDict        : TAbstractMultiMap<TKey, TValue>;

    protected
      { Hidden }
      function GetCount(): Cardinal; override;
    public
      { Constructor }
      constructor Create(const ADict: TAbstractMultiMap<TKey, TValue>);

      { Destructor }
      destructor Destroy(); override;

      { Property }
      property Count: Cardinal read GetCount;

      { IEnumerable/ ICollection support }
      function GetEnumerator(): IEnumerator<TKey>; override;

      { Copy-To }
      procedure CopyTo(var AArray: array of TKey); overload; override;
      procedure CopyTo(var AArray: array of TKey; const StartIndex: Cardinal); overload; override;

      { Enex Overrides }
      function Empty(): Boolean; override;
    end;

    { Generic MultiMap Values Collection }
    TValueCollection = class(TEnexCollection<TValue>)
    private
      FDict        : TAbstractMultiMap<TKey, TValue>;

    protected

      { Hidden }
      function GetCount: Cardinal; override;
    public
      { Constructor }
      constructor Create(const ADict: TAbstractMultiMap<TKey, TValue>);

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
    FVer            : Cardinal;
    FKnownCount     : Cardinal;
    FKeyCollection  : IEnexCollection<TKey>;
    FValueCollection: IEnexCollection<TValue>;
    FDictionary     : IDictionary<TKey, IList<TValue>>;

  protected
    { To be used in descending classes }
    property Dictionary: IDictionary<TKey, IList<TValue>> read FDictionary;

    { Hidden }
    function GetCount(): Cardinal; override;

    { Key getter and setter }
    function GetItemList(const Key: TKey): IEnexIndexedCollection<TValue>;

    { To be overriden }
    function CreateDictionary(const AKeyType: IType<TKey>): IDictionary<TKey, IList<TValue>>; virtual; abstract;
    function CreateList(const AValueType: IType<TValue>): IList<TValue>; virtual; abstract;

  public
    { Constructors }
    constructor Create(); overload;
    constructor Create(const AEnumerable: IEnumerable<TKeyValuePair<TKey,TValue>>); overload;
    constructor Create(const AArray: array of TKeyValuePair<TKey,TValue>); overload;
    constructor Create(const AArray: TDynamicArray<TKeyValuePair<TKey, TValue>>); overload;
    constructor Create(const AArray: TFixedArray<TKeyValuePair<TKey, TValue>>); overload;

    constructor Create(const AKeyType: IType<TKey>; const AValueType: IType<TValue>); overload;
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

    {  Modification }
    procedure Clear();

    procedure Add(const APair: TKeyValuePair<TKey, TValue>); overload;
    procedure Add(const AKey: TKey; const AValue: TValue); overload;

    procedure Remove(const AKey: TKey); overload;
    procedure Remove(const AKey: TKey; const AValue: TValue); overload;
    procedure Remove(const APair: TKeyValuePair<TKey, TValue>); overload;

    { Lookup }
    function ContainsKey(const AKey: TKey): Boolean;

    function ContainsValue(const AValue: TValue): Boolean; overload;
    function ContainsValue(const AKey: TKey; const AValue: TValue): Boolean; overload;
    function ContainsValue(const APair: TKeyValuePair<TKey, TValue>): Boolean; overload;

    { Try to get the value list }
    function TryGetValues(const AKey: TKey; out AValueList: IEnexIndexedCollection<TValue>): Boolean;

    { Properties }
    property Items[const Key: TKey]: IEnexIndexedCollection<TValue> read GetItemList; default;

    property Count: Cardinal read FKnownCount;
    property Keys: IEnexCollection<TKey> read FKeyCollection;
    property Values: IEnexCollection<TValue> read FValueCollection;

    { IEnumerable/ ICollection support }
    function GetEnumerator(): IEnumerator<TKeyValuePair<TKey,TValue>>; override;

    { Copy-To }
    procedure CopyTo(var AArray: array of TKeyValuePair<TKey,TValue>); overload; override;
    procedure CopyTo(var AArray: array of TKeyValuePair<TKey,TValue>; const StartIndex: Cardinal); overload; override;

    { Enex - associativity }
    function ValueForKey(const AKey: TKey): TValue; override;
    function KeyHasValue(const AKey: TKey; const AValue: TValue): Boolean; override;

    { Enex - selectors }
    function SelectKeys(): IEnexCollection<TKey>; override;
    function SelectValues(): IEnexCollection<TValue>; override;
  end;

  { Set-Map base class }
  TAbstractDistinctMultiMap<TKey, TValue> = class abstract(TEnexAssociativeCollection<TKey, TValue>, IDistinctMultiMap<TKey, TValue>)
  private
  type
    { Generic MultiMap Pairs Enumerator }
    TPairEnumerator = class(TEnumerator<TKeyValuePair<TKey,TValue>>)
    private
      FVer         : Cardinal;
      FDict        : TAbstractDistinctMultiMap<TKey, TValue>;
      FValue       : TKeyValuePair<TKey, TValue>;

      FSetEnum     : IEnumerator<TValue>;
      FDictEnum    : IEnumerator<TKeyValuePair<TKey, ISet<TValue>>>;
      FSet         : ISet<TValue>;

    public
      { Constructor }
      constructor Create(const ADict: TAbstractDistinctMultiMap<TKey, TValue>);

      { Destructor }
      destructor Destroy(); override;

      function GetCurrent(): TKeyValuePair<TKey,TValue>; override;
      function MoveNext(): Boolean; override;
    end;

    { Generic MultiMap Keys Enumerator }
    TKeyEnumerator = class(TEnumerator<TKey>)
    private
      FVer         : Cardinal;
      FDict        : TAbstractDistinctMultiMap<TKey, TValue>;
      FValue       : TKey;
      FDictEnum    : IEnumerator<TKey>;

    public
      { Constructor }
      constructor Create(const ADict: TAbstractDistinctMultiMap<TKey, TValue>);

      { Destructor }
      destructor Destroy(); override;

      function GetCurrent(): TKey; override;
      function MoveNext(): Boolean; override;
    end;

    { Generic MultiMap Values Enumerator }
    TValueEnumerator = class(TEnumerator<TValue>)
    private
      FVer         : Cardinal;
      FDict        : TAbstractDistinctMultiMap<TKey, TValue>;
      FValue       : TValue;

      FDictEnum    : IEnumerator<ISet<TValue>>;
      FSetEnum     : IEnumerator<TValue>;
      FSet         : ISet<TValue>;

    public
      { Constructor }
      constructor Create(const ADict: TAbstractDistinctMultiMap<TKey, TValue>);

      { Destructor }
      destructor Destroy(); override;

      function GetCurrent(): TValue; override;
      function MoveNext(): Boolean; override;
    end;

    { Generic MultiMap Keys Collection }
    TKeyCollection = class(TEnexCollection<TKey>)
    private
      FDict        : TAbstractDistinctMultiMap<TKey, TValue>;

    protected
      { Hidden }
      function GetCount(): Cardinal; override;
    public
      { Constructor }
      constructor Create(const ADict: TAbstractDistinctMultiMap<TKey, TValue>);

      { Destructor }
      destructor Destroy(); override;

      { Property }
      property Count: Cardinal read GetCount;

      { IEnumerable/ ICollection support }
      function GetEnumerator(): IEnumerator<TKey>; override;

      { Copy-To }
      procedure CopyTo(var AArray: array of TKey); overload; override;
      procedure CopyTo(var AArray: array of TKey; const StartIndex: Cardinal); overload; override;

      { Enex Overrides }
      function Empty(): Boolean; override;
    end;

    { Generic MultiMap Values Collection }
    TValueCollection = class(TEnexCollection<TValue>)
    private
      FDict        : TAbstractDistinctMultiMap<TKey, TValue>;

    protected
      { Hidden }
      function GetCount: Cardinal; override;

    public
      { Constructor }
      constructor Create(const ADict: TAbstractDistinctMultiMap<TKey, TValue>);

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
    FVer            : Cardinal;
    FKnownCount     : Cardinal;
    FKeyCollection  : IEnexCollection<TKey>;
    FValueCollection: IEnexCollection<TValue>;
    FDictionary     : IDictionary<TKey, ISet<TValue>>;

  protected
    { To be used in descending classes }
    property Dictionary: IDictionary<TKey, ISet<TValue>> read FDictionary;

    { Hidden }
    function GetCount(): Cardinal; override;

    { Key getter and setter }
    function GetItemList(const Key: TKey): IEnexCollection<TValue>;

    { To be overriden }
    function CreateDictionary(const AKeyType: IType<TKey>): IDictionary<TKey, ISet<TValue>>; virtual; abstract;
    function CreateSet(const AValueType: IType<TValue>): ISet<TValue>; virtual; abstract;

  public
    { Constructors }
    constructor Create(); overload;
    constructor Create(const AEnumerable: IEnumerable<TKeyValuePair<TKey,TValue>>); overload;
    constructor Create(const AArray: array of TKeyValuePair<TKey,TValue>); overload;
    constructor Create(const AArray: TDynamicArray<TKeyValuePair<TKey, TValue>>); overload;
    constructor Create(const AArray: TFixedArray<TKeyValuePair<TKey, TValue>>); overload;

    constructor Create(const AKeyType: IType<TKey>; const AValueType: IType<TValue>); overload;
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

    {  Modification }
    procedure Clear();

    procedure Add(const APair: TKeyValuePair<TKey, TValue>); overload;
    procedure Add(const AKey: TKey; const AValue: TValue); overload;

    procedure Remove(const AKey: TKey); overload;
    procedure Remove(const AKey: TKey; const AValue: TValue); overload;
    procedure Remove(const APair: TKeyValuePair<TKey, TValue>); overload;

    { Lookup }
    function ContainsKey(const AKey: TKey): Boolean;

    function ContainsValue(const AValue: TValue): Boolean; overload;
    function ContainsValue(const AKey: TKey; const AValue: TValue): Boolean; overload;
    function ContainsValue(const APair: TKeyValuePair<TKey, TValue>): Boolean; overload;

    { Try to get the value list }
    function TryGetValues(const AKey: TKey; out AValueSet: IEnexCollection<TValue>): Boolean;

    { Properties }
    property Items[const Key: TKey]: IEnexCollection<TValue> read GetItemList; default;

    property Count: Cardinal read FKnownCount;
    property Keys: IEnexCollection<TKey> read FKeyCollection;
    property Values: IEnexCollection<TValue> read FValueCollection;

    { IEnumerable/ ICollection support }
    function GetEnumerator(): IEnumerator<TKeyValuePair<TKey,TValue>>; override;

    { Copy-To }
    procedure CopyTo(var AArray: array of TKeyValuePair<TKey,TValue>); overload; override;
    procedure CopyTo(var AArray: array of TKeyValuePair<TKey,TValue>; const StartIndex: Cardinal); overload; override;

    { Enex - associativity }
    function ValueForKey(const AKey: TKey): TValue; override;
    function KeyHasValue(const AKey: TKey; const AValue: TValue): Boolean; override;

    { Enex - selectors }
    function SelectKeys(): IEnexCollection<TKey>; override;
    function SelectValues(): IEnexCollection<TValue>; override;
  end;

  { Class that represents the bidirectional map }
  TAbstractBidiMap<TKey, TValue> = class abstract(TEnexAssociativeCollection<TKey, TValue>, IBidiMap<TKey, TValue>)
  private
    FByKeyMap: IDistinctMultiMap<TKey, TValue>;
    FByValueMap: IDistinctMultiMap<TValue, TKey>;

    { Got from the underlying collections }
    FValueCollection: IEnexCollection<TValue>;
    FKeyCollection: IEnexCollection<TKey>;

  protected
    { Read-only access to the maps }
    property ByKeyMap: IDistinctMultiMap<TKey, TValue> read FByKeyMap;
    property ByValueMap: IDistinctMultiMap<TValue, TKey> read FByValueMap;

    { Override to provide map implemenatations }
    function CreateKeyMap(const AKeyType: IType<TKey>;
      const AValueType: IType<TValue>): IDistinctMultiMap<TKey, TValue>; virtual; abstract;

    function CreateValueMap(const AValueType: IType<TValue>;
      const AKeyType: IType<TKey>): IDistinctMultiMap<TValue, TKey>; virtual; abstract;

    { Hidden }
    function GetCount(): Cardinal; override;

    { Getters for the keys and values }
    function GetKeyList(const AValue: TValue): IEnexCollection<TKey>;
    function GetValueList(const AKey: TKey): IEnexCollection<TValue>;
  public
    { Constructors }
    constructor Create(); overload;
    constructor Create(const AEnumerable: IEnumerable<TKeyValuePair<TKey,TValue>>); overload;
    constructor Create(const AArray: array of TKeyValuePair<TKey,TValue>); overload;
    constructor Create(const AArray: TDynamicArray<TKeyValuePair<TKey, TValue>>); overload;
    constructor Create(const AArray: TFixedArray<TKeyValuePair<TKey, TValue>>); overload;

    constructor Create(const AKeyType: IType<TKey>; const AValueType: IType<TValue>); overload;
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

    { Clearing }
    procedure Clear();

    { Adding }
    procedure Add(const APair: TKeyValuePair<TKey, TValue>); overload;
    procedure Add(const AKey: TKey; const AValue: TValue); overload;

    { Removal }
    procedure RemoveKey(const AKey: TKey);
    procedure Remove(const AKey: TKey); overload;
    procedure RemoveValue(const AValue: TValue);

    procedure Remove(const AKey: TKey; const AValue: TValue); overload;
    procedure Remove(const APair: TKeyValuePair<TKey, TValue>); overload;

    { Lookup }
    function ContainsKey(const AKey: TKey): Boolean;
    function ContainsValue(const AValue: TValue): Boolean;

    function ContainsPair(const AKey: TKey; const AValue: TValue): Boolean; overload;
    function ContainsPair(const APair: TKeyValuePair<TKey, TValue>): Boolean; overload;

    { Properties }
    property ByKey[const AKey: TKey]: IEnexCollection<TValue> read GetValueList;
    property ByValue[const AValue: TValue]: IEnexCollection<TKey> read GetKeyList;

    property Keys: IEnexCollection<TKey> read FKeyCollection;
    property Values: IEnexCollection<TValue> read FValueCollection;

    property Count: Cardinal read GetCount;

    { IEnumerable/ ICollection support }
    function GetEnumerator(): IEnumerator<TKeyValuePair<TKey, TValue>>; override;

    { Copy-To }
    procedure CopyTo(var AArray: array of TKeyValuePair<TKey,TValue>); overload; override;
    procedure CopyTo(var AArray: array of TKeyValuePair<TKey,TValue>; const StartIndex: Cardinal); overload; override;

    { Enex - associativity }
    function ValueForKey(const AKey: TKey): TValue; override;
    function KeyHasValue(const AKey: TKey; const AValue: TValue): Boolean; override;

    { Enex - selectors }
    function SelectKeys(): IEnexCollection<TKey>; override;
    function SelectValues(): IEnexCollection<TValue>; override;
  end;

  { Bag base class }
  TAbstractBag<T> = class(TEnexCollection<T>, IBag<T>)
  type
    { Enumerator }
    TEnumerator = class(TEnumerator<T>)
    private
      FVer         : Cardinal;
      FDict        : TAbstractBag<T>;
      FCurrentKV   : IEnumerator<TKeyValuePair<T, Cardinal>>;
      FCurrentCount: Cardinal;
      FValue       : T;

    public
      { Constructor }
      constructor Create(const ADict: TAbstractBag<T>);

      { Destructor }
      destructor Destroy(); override;

      function GetCurrent(): T; override;
      function MoveNext(): Boolean; override;
    end;

  var
    FDictionary: IDictionary<T, Cardinal>;
    FVer: Cardinal;
    FKnownCount: Cardinal;

  protected
    { To be used in descending classes }
    property Dictionary: IDictionary<T, Cardinal> read FDictionary;

    { Hidden }
    function GetCount(): Cardinal; override;

    { IBag }
    function GetItemCount(const AnItem: T): Cardinal;
    procedure SetItemCount(const AnItem: T; const Value: Cardinal);

    { To be overriden }
    function CreateDictionary(const ATyoe: IType<T>): IDictionary<T, Cardinal>; virtual; abstract;
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

    procedure Add(const AnItem: T; const Count: Cardinal = 1);
    procedure Remove(const AnItem: T; const Count: Cardinal = 1);
    procedure RemoveAll(const AnItem: T);

    { Lookup }
    function Contains(const AnItem: T; const Count: Cardinal = 1): Boolean;

    { Properties }
    property Counts[const AnItem: T]: Cardinal read GetItemCount write SetItemCount; default;
    property Count: Cardinal read FKnownCount;

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
    function Any(const APredicate: TFunc<T, Boolean>): Boolean; override;
    function All(const APredicate: TFunc<T, Boolean>): Boolean; override;
  end;

implementation

{ TAbstractMultiMap<TKey, TValue> }

procedure TAbstractMultiMap<TKey, TValue>.Add(const APair: TKeyValuePair<TKey, TValue>);
begin
  { Call the other add }
  Add(APair.Key, APair.Value);
end;

procedure TAbstractMultiMap<TKey, TValue>.Add(const AKey: TKey; const AValue: TValue);
var
  List: IList<TValue>;
begin
  { Try to look-up what we need. Create a new list and add it if required. }
  if not FDictionary.TryGetValue(AKey, List) then
  begin
    List := CreateList(FValueType);
    FDictionary[AKey] := List;
  end;

  { Add the new element to the list }
  List.Add(AValue);

  { Increase the version }
  Inc(FKnownCount);
  Inc(FVer);
end;

procedure TAbstractMultiMap<TKey, TValue>.Clear;
var
  List: IList<TValue>;
begin
  if (FDictionary <> nil) then
    { Simply clear out the dictionary }
    FDictionary.Clear();

  { Increase the version }
  FKnownCount := 0;
  Inc(FVer);
end;

function TAbstractMultiMap<TKey, TValue>.ContainsKey(const AKey: TKey): Boolean;
begin
  { Delegate to the dictionary object }
  Result := FDictionary.ContainsKey(AKey);
end;

function TAbstractMultiMap<TKey, TValue>.ContainsValue(const AKey: TKey; const AValue: TValue): Boolean;
var
  List: IList<TValue>;
begin
  { Try to find .. otherwise fail! }
  if FDictionary.TryGetValue(AKey, List) then
    Result := List.Contains(AValue)
  else
    Result := false;
end;

function TAbstractMultiMap<TKey, TValue>.ContainsValue(const APair: TKeyValuePair<TKey, TValue>): Boolean;
begin
  { Call upper function }
  Result := ContainsValue(APair.Key, APair.Value);
end;

function TAbstractMultiMap<TKey, TValue>.ContainsValue(const AValue: TValue): Boolean;
var
  List: IList<TValue>;
begin
  { Iterate over the dictionary }
  for List in FDictionary.Values do
  begin
    { Is there anything there? }
    if List.Contains(AValue) then
      Exit(true);
  end;

  { Nothing found }
  Result := false;
end;

procedure TAbstractMultiMap<TKey, TValue>.CopyTo(var AArray: array of TKeyValuePair<TKey, TValue>);
begin
  { Call the more generic function }
  CopyTo(AArray, 0);
end;

procedure TAbstractMultiMap<TKey, TValue>.CopyTo(
  var AArray: array of TKeyValuePair<TKey, TValue>; const StartIndex: Cardinal);
var
  Key: TKey;
  List: IList<TValue>;
  X, I: Cardinal;
begin
  { Check for indexes }
  if StartIndex >= Cardinal(Length(AArray)) then
    ExceptionHelper.Throw_ArgumentOutOfRangeError('StartIndex');

  if (Cardinal(Length(AArray)) - StartIndex) < Count then
     ExceptionHelper.Throw_ArgumentOutOfSpaceError('AArray');

  X := StartIndex;

  { Iterate over all lists and copy thtm to array }
  for Key in FDictionary.Keys do
  begin
    List := FDictionary[Key];

    if List.Count > 0 then
      for I := 0 to List.Count - 1 do
        AArray[X + I] := TKeyValuePair<TKey, TValue>.Create(Key, List[I]);

    Inc(X, List.Count);
  end;
end;

constructor TAbstractMultiMap<TKey, TValue>.Create;
begin
  Create(TType<TKey>.Default, TType<TValue>.Default);
end;

constructor TAbstractMultiMap<TKey, TValue>.Create(
  const AEnumerable: IEnumerable<TKeyValuePair<TKey, TValue>>);
begin
  Create(TType<TKey>.Default, TType<TValue>.Default, AEnumerable);
end;

constructor TAbstractMultiMap<TKey, TValue>.Create(
  const AKeyType: IType<TKey>;
  const AValueType: IType<TValue>);
begin
  { Initialize instance }
  if (AKeyType = nil) then
     ExceptionHelper.Throw_ArgumentNilError('AKeyType');

  if (AValueType = nil) then
     ExceptionHelper.Throw_ArgumentNilError('AValueType');

  { Insatll the types }
  InstallTypes(AKeyType, AValueType);

  { Create the dictionary }
  FDictionary := CreateDictionary(FKeyType);

  FKeyCollection := TKeyCollection.Create(Self);
  FValueCollection := TValueCollection.Create(Self);

  FKnownCount := 0;
  FVer := 0;
end;

constructor TAbstractMultiMap<TKey, TValue>.Create(const AKeyType: IType<TKey>;
  const AValueType: IType<TValue>;
  const AEnumerable: IEnumerable<TKeyValuePair<TKey, TValue>>);
var
  V: TKeyValuePair<TKey, TValue>;
begin
  { Call upper constructor }
  Create(AKeyType, AValueType);

  if (AEnumerable = nil) then
     ExceptionHelper.Throw_ArgumentNilError('AEnumerable');

  { Pump in all items }
  for V in AEnumerable do
  begin
{$IFNDEF BUG_GENERIC_INCOMPAT_TYPES}
    Add(V);
{$ELSE}
    Add(V.Key, V.Value);
{$ENDIF}
  end;
end;

constructor TAbstractMultiMap<TKey, TValue>.Create(
  const AArray: array of TKeyValuePair<TKey, TValue>);
begin
  Create(TType<TKey>.Default, TType<TValue>.Default, AArray);
end;

constructor TAbstractMultiMap<TKey, TValue>.Create(
  const AKeyType: IType<TKey>;
  const AValueType: IType<TValue>;
  const AArray: array of TKeyValuePair<TKey, TValue>);
var
  I: Integer;
begin
  { Call upper constructor }
  Create(AKeyType, AValueType);

  { Copy all items in }
  for I := 0 to Length(AArray) - 1 do
  begin
    Add(AArray[I]);
  end;
end;


constructor TAbstractMultiMap<TKey, TValue>.Create(const AArray: TFixedArray<TKeyValuePair<TKey, TValue>>);
begin
  Create(TType<TKey>.Default, TType<TValue>.Default, AArray);
end;

constructor TAbstractMultiMap<TKey, TValue>.Create(const AArray: TDynamicArray<TKeyValuePair<TKey, TValue>>);
begin
  Create(TType<TKey>.Default, TType<TValue>.Default, AArray);
end;

constructor TAbstractMultiMap<TKey, TValue>.Create(const AKeyType: IType<TKey>;
  const AValueType: IType<TValue>;
  const AArray: TFixedArray<TKeyValuePair<TKey, TValue>>);
var
  I: Cardinal;
begin
  { Call upper constructor }
  Create(AKeyType, AValueType);

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

constructor TAbstractMultiMap<TKey, TValue>.Create(const AKeyType: IType<TKey>;
  const AValueType: IType<TValue>;
  const AArray: TDynamicArray<TKeyValuePair<TKey, TValue>>);
var
  I: Cardinal;
begin
  { Call upper constructor }
  Create(AKeyType, AValueType);

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


destructor TAbstractMultiMap<TKey, TValue>.Destroy;
begin
  { Clear first }
  Clear();

  inherited;
end;

function TAbstractMultiMap<TKey, TValue>.GetCount: Cardinal;
begin
  Result := FKnownCount;
end;

function TAbstractMultiMap<TKey, TValue>.GetEnumerator: IEnumerator<TKeyValuePair<TKey, TValue>>;
begin
  Result := TPairEnumerator.Create(Self);
end;

function TAbstractMultiMap<TKey, TValue>.GetItemList(const Key: TKey): IEnexIndexedCollection<TValue>;
var
  List: IList<TValue>;
begin
  if not FDictionary.TryGetValue(Key, List) then
    ExceptionHelper.Throw_KeyNotFoundError('Key');

  Result := List;
end;

function TAbstractMultiMap<TKey, TValue>.KeyHasValue(const AKey: TKey; const AValue: TValue): Boolean;
begin
  Result := ContainsValue(AKey, AValue);
end;

procedure TAbstractMultiMap<TKey, TValue>.Remove(const AKey: TKey; const AValue: TValue);
var
  List: IList<TValue>;
begin
  { Simply remove the value from the list at key }
  if FDictionary.TryGetValue(AKey, List) then
  begin
    if List.Contains(AValue) then
    begin
      List.Remove(AValue);

      { Kill the list for one element }
      if List.Count = 0 then
        FDictionary.Remove(AKey);

      Dec(FKnownCount, 1);

      { Increase the version }
      Inc(FVer);
    end;
  end;
end;

procedure TAbstractMultiMap<TKey, TValue>.Remove(const APair: TKeyValuePair<TKey, TValue>);
begin
  { Call upper function }
  Remove(APair.Key, APair.Value);
end;

function TAbstractMultiMap<TKey, TValue>.SelectKeys: IEnexCollection<TKey>;
begin
  Result := Keys;
end;

function TAbstractMultiMap<TKey, TValue>.SelectValues: IEnexCollection<TValue>;
begin
  Result := Values;
end;

function TAbstractMultiMap<TKey, TValue>.TryGetValues(const AKey: TKey;
  out AValueList: IEnexIndexedCollection<TValue>): Boolean;
var
  LList: IList<TValue>;
begin
  { Use the internal stuff }
  Result := FDictionary.TryGetValue(AKey, LList);

  if Result then
    AValueList := LList;
end;

function TAbstractMultiMap<TKey, TValue>.ValueForKey(const AKey: TKey): TValue;
begin
  Result := GetItemList(AKey)[0];
end;

procedure TAbstractMultiMap<TKey, TValue>.Remove(const AKey: TKey);
var
  List: IList<TValue>;
begin
  if FDictionary.TryGetValue(AKey, List) then
    Dec(FKnownCount, List.Count);

  { Simply remove the element. The list should be auto-magically collected also }
  FDictionary.Remove(AKey);

  { Increase the version }
  Inc(FVer);
end;

{ TAbstractMultiMap<TKey, TValue>.TPairEnumerator }

constructor TAbstractMultiMap<TKey, TValue>.TPairEnumerator.Create(const ADict: TAbstractMultiMap<TKey, TValue>);
begin
  { Initialize }
  FDict := ADict;
  KeepObjectAlive(FDict);

  FVer := ADict.FVer;

  { Get the enumerator }
  FListIndex := 0;
  FDictEnum := FDict.FDictionary.GetEnumerator();
  FList := nil;
end;

destructor TAbstractMultiMap<TKey, TValue>.TPairEnumerator.Destroy;
begin
  ReleaseObject(FDict);
  inherited;
end;

function TAbstractMultiMap<TKey, TValue>.TPairEnumerator.GetCurrent: TKeyValuePair<TKey,TValue>;
begin
  if FVer <> FDict.FVer then
     ExceptionHelper.Throw_CollectionChangedError();

  Result := FValue;
end;

function TAbstractMultiMap<TKey, TValue>.TPairEnumerator.MoveNext: Boolean;
begin
  { Repeat until something happens }
  while True do
  begin
    if FVer <> FDict.FVer then
       ExceptionHelper.Throw_CollectionChangedError();

    { We're still in the same KV? }
    if (FList <> nil) and (FListIndex < FList.Count) then
    begin
      { Next element }
      FValue := TKeyValuePair<TKey, TValue>.Create(FDictEnum.Current.Key, FList[FListIndex]);

      Inc(FListIndex);
      Result := true;

      Exit;
    end;

    { Get the next KV pair from the dictionary }
    Result := FDictEnum.MoveNext();
    if not Result then
    begin
      FList := nil;
      Exit;
    end;

    { Reset the list }
    FListIndex := 0;
    FList := FDictEnum.Current.Value;
  end;
end;

{ TAbstractMultiMap<TKey, TValue>.TKeyEnumerator }

constructor TAbstractMultiMap<TKey, TValue>.TKeyEnumerator.Create(const ADict: TAbstractMultiMap<TKey, TValue>);
begin
  { Initialize }
  FDict := ADict;
  KeepObjectAlive(FDict);

  FVer := ADict.FVer;
  FValue := default(TKey);

  { Create enumerator }
  FDictEnum := FDict.FDictionary.Keys.GetEnumerator();
end;

destructor TAbstractMultiMap<TKey, TValue>.TKeyEnumerator.Destroy;
begin
  ReleaseObject(FDict);
  inherited;
end;

function TAbstractMultiMap<TKey, TValue>.TKeyEnumerator.GetCurrent: TKey;
begin
  if FVer <> FDict.FVer then
     ExceptionHelper.Throw_CollectionChangedError();

  Result := FValue;
end;

function TAbstractMultiMap<TKey, TValue>.TKeyEnumerator.MoveNext: Boolean;
begin
  if FVer <> FDict.FVer then
     ExceptionHelper.Throw_CollectionChangedError();

  { Move next and get the value }
  Result := FDictEnum.MoveNext();
  if Result then
    FValue := FDictEnum.Current;
end;


{ TAbstractMultiMap<TKey, TValue>.TValueEnumerator }

constructor TAbstractMultiMap<TKey, TValue>.TValueEnumerator.Create(const ADict: TAbstractMultiMap<TKey, TValue>);
begin
  { Initialize }
  FDict := ADict;
  KeepObjectAlive(FDict);

  FVer := ADict.FVer;

  { Get the enumerator }
  FListIndex := 0;
  FDictEnum := FDict.FDictionary.Values.GetEnumerator();
  FList := nil;
end;

destructor TAbstractMultiMap<TKey, TValue>.TValueEnumerator.Destroy;
begin
  ReleaseObject(FDict);
  inherited;
end;

function TAbstractMultiMap<TKey, TValue>.TValueEnumerator.GetCurrent: TValue;
begin
  if FVer <> FDict.FVer then
     ExceptionHelper.Throw_CollectionChangedError();

  Result := FValue;
end;

function TAbstractMultiMap<TKey, TValue>.TValueEnumerator.MoveNext: Boolean;
begin
  { Repeat until something happens }
  while True do
  begin
    if FVer <> FDict.FVer then
       ExceptionHelper.Throw_CollectionChangedError();

    { We're still in the same KV? }
    if (FList <> nil) and (FListIndex < FList.Count) then
    begin
      { Next element }
      FValue := FList[FListIndex];

      Inc(FListIndex);
      Result := true;

      Exit;
    end;

    { Get the next KV pair from the dictionary }
    Result := FDictEnum.MoveNext();
    if not Result then
    begin
      FList := nil;
      Exit;
    end;

    { Reset the list }
    FListIndex := 0;
    FList := FDictEnum.Current;
  end;
end;

{ TAbstractMultiMap<TKey, TValue>.TKeyCollection }

constructor TAbstractMultiMap<TKey, TValue>.TKeyCollection.Create(const ADict: TAbstractMultiMap<TKey, TValue>);
begin
  { Initialize }
  FDict := ADict;

  InstallType(ADict.KeyType);
end;

destructor TAbstractMultiMap<TKey, TValue>.TKeyCollection.Destroy;
begin
  inherited;
end;

function TAbstractMultiMap<TKey, TValue>.TKeyCollection.Empty: Boolean;
begin
  Result := (FDict.FDictionary.Count = 0);
end;

function TAbstractMultiMap<TKey, TValue>.TKeyCollection.GetCount: Cardinal;
begin
  { Number of elements is the same as key }
  Result := FDict.FDictionary.Count;
end;

function TAbstractMultiMap<TKey, TValue>.TKeyCollection.GetEnumerator: IEnumerator<TKey>;
begin
  Result := TKeyEnumerator.Create(Self.FDict);
end;

procedure TAbstractMultiMap<TKey, TValue>.TKeyCollection.CopyTo(var AArray: array of TKey);
begin
  { Call more generic function }
  CopyTo(AArray, 0);
end;

procedure TAbstractMultiMap<TKey, TValue>.TKeyCollection.CopyTo(var AArray: array of TKey; const StartIndex: Cardinal);
begin
  { Check for indexes }
  if StartIndex >= Cardinal(Length(AArray)) then
    ExceptionHelper.Throw_ArgumentOutOfRangeError('StartIndex');

  if (Cardinal(Length(AArray)) - StartIndex) < FDict.FDictionary.Count then
     ExceptionHelper.Throw_ArgumentOutOfSpaceError('AArray');

  { Simply copy using the dictionary provided methods }
  FDict.FDictionary.Keys.CopyTo(AArray, StartIndex);
end;

{ TAbstractMultiMap<TKey, TValue>.TValueCollection }

constructor TAbstractMultiMap<TKey, TValue>.TValueCollection.Create(const ADict: TAbstractMultiMap<TKey, TValue>);
begin
  { Initialize }
  FDict := ADict;

  InstallType(ADict.ValueType);
end;

destructor TAbstractMultiMap<TKey, TValue>.TValueCollection.Destroy;
begin
  inherited;
end;

function TAbstractMultiMap<TKey, TValue>.TValueCollection.Empty: Boolean;
begin
  Result := (FDict.FDictionary.Count = 0);
end;

function TAbstractMultiMap<TKey, TValue>.TValueCollection.GetCount: Cardinal;
begin
  { Number of elements is different use the count provided by the dictionary }
  Result := FDict.Count;
end;

function TAbstractMultiMap<TKey, TValue>.TValueCollection.GetEnumerator: IEnumerator<TValue>;
begin
  Result := TValueEnumerator.Create(Self.FDict);
end;

procedure TAbstractMultiMap<TKey, TValue>.TValueCollection.CopyTo(var AArray: array of TValue);
begin
  { Call more generic function }
  CopyTo(AArray, 0);
end;

procedure TAbstractMultiMap<TKey, TValue>.TValueCollection.CopyTo(var AArray: array of TValue; const StartIndex: Cardinal);
var
  List: IList<TValue>;
  X, I: Cardinal;
begin
  { Check for indexes }
  if StartIndex >= Cardinal(Length(AArray)) then
    ExceptionHelper.Throw_ArgumentOutOfRangeError('StartIndex');

  if (Cardinal(Length(AArray)) - StartIndex) < FDict.Count then
     ExceptionHelper.Throw_ArgumentOutOfSpaceError('AArray');

  X := StartIndex;

  { Iterate over all lists and copy thtm to array }
  for List in FDict.FDictionary.Values do
  begin
    if List.Count > 0 then
      for I := 0 to List.Count - 1 do
        AArray[X + I] := List[I];

    Inc(X, List.Count);
  end;
end;



{ TAbstractDistinctMultiMap<TKey, TValue> }

procedure TAbstractDistinctMultiMap<TKey, TValue>.Add(const APair: TKeyValuePair<TKey, TValue>);
begin
  { Call the other add }
  Add(APair.Key, APair.Value);
end;

procedure TAbstractDistinctMultiMap<TKey, TValue>.Add(const AKey: TKey; const AValue: TValue);
var
  LSet: ISet<TValue>;
begin
  { Try to look-up what we need. Create a new list and add it if required. }
  if not FDictionary.TryGetValue(AKey, LSet) then
  begin
    LSet := CreateSet(FValueType);
    FDictionary[AKey] := LSet;
  end;

  { Add the new element to the list }
  if not LSet.Contains(AValue) then
  begin
    LSet.Add(AValue);

    { Increase the version }
    Inc(FKnownCount);
    Inc(FVer);
  end;
end;

procedure TAbstractDistinctMultiMap<TKey, TValue>.Clear;
var
  List: IList<TValue>;
begin
  if (FDictionary <> nil) then
    { Simply clear out the dictionary }
    FDictionary.Clear();

  { Increase the version }
  FKnownCount := 0;
  Inc(FVer);
end;

function TAbstractDistinctMultiMap<TKey, TValue>.ContainsKey(const AKey: TKey): Boolean;
begin
  { Delegate to the dictionary object }
  Result := FDictionary.ContainsKey(AKey);
end;

function TAbstractDistinctMultiMap<TKey, TValue>.ContainsValue(const AKey: TKey; const AValue: TValue): Boolean;
var
  LSet: ISet<TValue>;
begin
  { Try to find .. otherwise fail! }
  if FDictionary.TryGetValue(AKey, LSet) then
    Result := LSet.Contains(AValue)
  else
    Result := false;
end;

function TAbstractDistinctMultiMap<TKey, TValue>.ContainsValue(const APair: TKeyValuePair<TKey, TValue>): Boolean;
begin
  { Call upper function }
  Result := ContainsValue(APair.Key, APair.Value);
end;

function TAbstractDistinctMultiMap<TKey, TValue>.ContainsValue(const AValue: TValue): Boolean;
var
  LSet: ISet<TValue>;
begin
  { Iterate over the dictionary }
  for LSet in FDictionary.Values do
  begin
    { Is there anything there? }
    if LSet.Contains(AValue) then
      Exit(true);
  end;

  { Nothing found }
  Result := false;
end;

procedure TAbstractDistinctMultiMap<TKey, TValue>.CopyTo(var AArray: array of TKeyValuePair<TKey, TValue>);
begin
  { Call the more generic function }
  CopyTo(AArray, 0);
end;

procedure TAbstractDistinctMultiMap<TKey, TValue>.CopyTo(
  var AArray: array of TKeyValuePair<TKey, TValue>; const StartIndex: Cardinal);
var
  Key: TKey;
  Value: TValue;
  LSet: ISet<TValue>;
  X: Cardinal;
begin
  { Check for indexes }
  if StartIndex >= Cardinal(Length(AArray)) then
    ExceptionHelper.Throw_ArgumentOutOfRangeError('StartIndex');

  if (Cardinal(Length(AArray)) - StartIndex) < Count then
     ExceptionHelper.Throw_ArgumentOutOfSpaceError('AArray');

  X := StartIndex;

  { Iterate over all lists and copy thtm to array }
  for Key in FDictionary.Keys do
  begin
    LSet := FDictionary[Key];

    for Value in LSet do
    begin
      AArray[X] := TKeyValuePair<TKey, TValue>.Create(Key, Value);
      Inc(X);
    end;
  end;
end;

constructor TAbstractDistinctMultiMap<TKey, TValue>.Create;
begin
  Create(TType<TKey>.Default, TType<TValue>.Default);
end;

constructor TAbstractDistinctMultiMap<TKey, TValue>.Create(
  const AEnumerable: IEnumerable<TKeyValuePair<TKey, TValue>>);
begin
  Create(TType<TKey>.Default, TType<TValue>.Default, AEnumerable);
end;

constructor TAbstractDistinctMultiMap<TKey, TValue>.Create(
  const AKeyType: IType<TKey>;
  const AValueType: IType<TValue>);
begin
  { Initialize instance }
  if (AKeyType = nil) then
     ExceptionHelper.Throw_ArgumentNilError('AKeyType');

  if (AValueType = nil) then
     ExceptionHelper.Throw_ArgumentNilError('AValueType');

  { Install the types }
  InstallTypes(AKeyType, AValueType);

  { Create the dictionary }
  FDictionary := CreateDictionary(FKeyType);

  FKeyCollection := TKeyCollection.Create(Self);
  FValueCollection := TValueCollection.Create(Self);

  FKnownCount := 0;
  FVer := 0;
end;

constructor TAbstractDistinctMultiMap<TKey, TValue>.Create(const AKeyType: IType<TKey>;
  const AValueType: IType<TValue>;
  const AEnumerable: IEnumerable<TKeyValuePair<TKey, TValue>>);
var
  V: TKeyValuePair<TKey, TValue>;
begin
  { Call upper constructor }
  Create(AKeyType, AValueType);

  if (AEnumerable = nil) then
     ExceptionHelper.Throw_ArgumentNilError('AEnumerable');

  { Pump in all items }
  for V in AEnumerable do
  begin
{$IFNDEF BUG_GENERIC_INCOMPAT_TYPES}
    Add(V);
{$ELSE}
    Add(V.Key, V.Value);
{$ENDIF}
  end;
end;

constructor TAbstractDistinctMultiMap<TKey, TValue>.Create(
  const AArray: array of TKeyValuePair<TKey, TValue>);
begin
  Create(TType<TKey>.Default, TType<TValue>.Default, AArray);
end;

constructor TAbstractDistinctMultiMap<TKey, TValue>.Create(
  const AKeyType: IType<TKey>;
  const AValueType: IType<TValue>;
  const AArray: array of TKeyValuePair<TKey, TValue>);
var
  I: Integer;
begin
  { Call upper constructor }
  Create(AKeyType, AValueType);

  { Copy all items in }
  for I := 0 to Length(AArray) - 1 do
  begin
    Add(AArray[I]);
  end;
end;


constructor TAbstractDistinctMultiMap<TKey, TValue>.Create(const AArray: TFixedArray<TKeyValuePair<TKey, TValue>>);
begin
  Create(TType<TKey>.Default, TType<TValue>.Default, AArray);
end;

constructor TAbstractDistinctMultiMap<TKey, TValue>.Create(const AArray: TDynamicArray<TKeyValuePair<TKey, TValue>>);
begin
  Create(TType<TKey>.Default, TType<TValue>.Default, AArray);
end;

constructor TAbstractDistinctMultiMap<TKey, TValue>.Create(const AKeyType: IType<TKey>;
  const AValueType: IType<TValue>;
  const AArray: TFixedArray<TKeyValuePair<TKey, TValue>>);
var
  I: Cardinal;
begin
  { Call upper constructor }
  Create(AKeyType, AValueType);

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

constructor TAbstractDistinctMultiMap<TKey, TValue>.Create(const AKeyType: IType<TKey>;
  const AValueType: IType<TValue>;
  const AArray: TDynamicArray<TKeyValuePair<TKey, TValue>>);
var
  I: Cardinal;
begin
  { Call upper constructor }
  Create(AKeyType, AValueType);

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

destructor TAbstractDistinctMultiMap<TKey, TValue>.Destroy;
begin
  { Clear first }
  Clear();

  inherited;
end;

function TAbstractDistinctMultiMap<TKey, TValue>.GetCount: Cardinal;
begin
  Result := FKnownCount;
end;

function TAbstractDistinctMultiMap<TKey, TValue>.GetEnumerator: IEnumerator<TKeyValuePair<TKey, TValue>>;
begin
  Result := TPairEnumerator.Create(Self);
end;

function TAbstractDistinctMultiMap<TKey, TValue>.GetItemList(const Key: TKey): IEnexIndexedCollection<TValue>;
var
  LSet: ISet<TValue>;
begin
  if not FDictionary.TryGetValue(Key, LSet) then
    ExceptionHelper.Throw_KeyNotFoundError('Key');

  Result := LSet;
end;

function TAbstractDistinctMultiMap<TKey, TValue>.KeyHasValue(const AKey: TKey; const AValue: TValue): Boolean;
begin
  Result := ContainsValue(AKey, AValue);
end;

procedure TAbstractDistinctMultiMap<TKey, TValue>.Remove(const AKey: TKey; const AValue: TValue);
var
  LSet: ISet<TValue>;
begin
  { Simply remove the value from the list at key }
  if FDictionary.TryGetValue(AKey, LSet) then
  begin
    if LSet.Contains(AValue) then
    begin
      LSet.Remove(AValue);

      { Kill the list for one element }
      if LSet.Count = 0 then
        FDictionary.Remove(AKey);

      Dec(FKnownCount, 1);
    end;
  end;

  { Increase th version }
  Inc(FVer);
end;

procedure TAbstractDistinctMultiMap<TKey, TValue>.Remove(const APair: TKeyValuePair<TKey, TValue>);
begin
  { Call upper function }
  Remove(APair.Key, APair.Value);
end;

function TAbstractDistinctMultiMap<TKey, TValue>.SelectKeys: IEnexCollection<TKey>;
begin
  Result := Keys;
end;

function TAbstractDistinctMultiMap<TKey, TValue>.SelectValues: IEnexCollection<TValue>;
begin
  Result := Values;
end;

function TAbstractDistinctMultiMap<TKey, TValue>.TryGetValues(const AKey: TKey;
  out AValueSet: IEnexCollection<TValue>): Boolean;
var
  LSet: ISet<TValue>;
begin
  { Use the internal stuff }
  Result := FDictionary.TryGetValue(AKey, LSet);

  if Result then
    AValueSet := LSet;
end;

function TAbstractDistinctMultiMap<TKey, TValue>.ValueForKey(const AKey: TKey): TValue;
begin
  Result := GetItemList(AKey).First;
end;

procedure TAbstractDistinctMultiMap<TKey, TValue>.Remove(const AKey: TKey);
var
  LSet: ISet<TValue>;
begin
  if FDictionary.TryGetValue(AKey, LSet) then
    Dec(FKnownCount, LSet.Count);

  { Simply remove the element. The list should be auto-magically collected also }
  FDictionary.Remove(AKey);

  { Increase th version }
  Inc(FVer);
end;

{ TAbstractBidiMap<TKey, TValue> }

constructor TAbstractBidiMap<TKey, TValue>.Create(const AArray: TDynamicArray<TKeyValuePair<TKey, TValue>>);
begin
  Create(TType<TKey>.Default, TType<TValue>.Default, AArray);
end;

constructor TAbstractBidiMap<TKey, TValue>.Create(const AArray: TFixedArray<TKeyValuePair<TKey, TValue>>);
begin
  Create(TType<TKey>.Default, TType<TValue>.Default, AArray);
end;

constructor TAbstractBidiMap<TKey, TValue>.Create(const AArray: array of TKeyValuePair<TKey, TValue>);
begin
  Create(TType<TKey>.Default, TType<TValue>.Default, AArray);
end;

constructor TAbstractBidiMap<TKey, TValue>.Create;
begin
  Create(TType<TKey>.Default, TType<TValue>.Default);
end;

constructor TAbstractBidiMap<TKey, TValue>.Create(const AEnumerable: IEnumerable<TKeyValuePair<TKey, TValue>>);
begin
  Create(TType<TKey>.Default, TType<TValue>.Default, AEnumerable);
end;

constructor TAbstractBidiMap<TKey, TValue>.Create(const AKeyType: IType<TKey>;
  const AValueType: IType<TValue>;
  const AArray: TDynamicArray<TKeyValuePair<TKey, TValue>>);
var
  I: Cardinal;
begin
  { Call upper constructor }
  Create(AKeyType, AValueType);

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

procedure TAbstractBidiMap<TKey, TValue>.Add(const AKey: TKey; const AValue: TValue);
begin
  { Add the K/V pair to the maps }
  FByKeyMap.Add(AKey, AValue);
  FByValueMap.Add(AValue, AKey);
end;

procedure TAbstractBidiMap<TKey, TValue>.Add(const APair: TKeyValuePair<TKey, TValue>);
begin
  Add(APair.Key, APair.Value);
end;

procedure TAbstractBidiMap<TKey, TValue>.Clear;
begin
  if FByKeyMap <> nil then
    FByKeyMap.Clear;

  if FByValueMap <> nil then
    FByValueMap.Clear;
end;

function TAbstractBidiMap<TKey, TValue>.ContainsKey(const AKey: TKey): Boolean;
begin
  Result := FByKeyMap.ContainsKey(AKey);
end;

function TAbstractBidiMap<TKey, TValue>.ContainsPair(const APair: TKeyValuePair<TKey, TValue>): Boolean;
begin
  { The the by-key relation since it is correct always }
  Result := FByKeyMap.ContainsValue(APair.Key, APair.Value);
end;

function TAbstractBidiMap<TKey, TValue>.ContainsPair(const AKey: TKey; const AValue: TValue): Boolean;
begin
  { The the by-key relation since it is correct always }
  Result := FByKeyMap.ContainsValue(AKey, AValue);
end;

function TAbstractBidiMap<TKey, TValue>.ContainsValue(const AValue: TValue): Boolean;
begin
  Result := FByValueMap.ContainsKey(AValue);
end;

procedure TAbstractBidiMap<TKey, TValue>.CopyTo(var AArray: array of TKeyValuePair<TKey, TValue>; const StartIndex: Cardinal);
begin
  { Check for indexes }
  if StartIndex >= Cardinal(Length(AArray)) then
    ExceptionHelper.Throw_ArgumentOutOfRangeError('StartIndex');

  if (Cardinal(Length(AArray)) - StartIndex) < Count then
     ExceptionHelper.Throw_ArgumentOutOfSpaceError('AArray');

  { Call the underlying collection }
  FByKeyMap.CopyTo(AArray, StartIndex);
end;

procedure TAbstractBidiMap<TKey, TValue>.CopyTo(var AArray: array of TKeyValuePair<TKey, TValue>);
begin
  { Call the more generic function }
  CopyTo(AArray, 0);
end;

constructor TAbstractBidiMap<TKey, TValue>.Create(const AKeyType: IType<TKey>;
  const AValueType: IType<TValue>;
  const AArray: TFixedArray<TKeyValuePair<TKey, TValue>>);
var
  I: Cardinal;
begin
  { Call upper constructor }
  Create(AKeyType, AValueType);

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

destructor TAbstractBidiMap<TKey, TValue>.Destroy;
begin
  { Clear out the instance }
  Clear();

  inherited;
end;

function TAbstractBidiMap<TKey, TValue>.GetCount: Cardinal;
begin
  { The cound follows the map properties }
  Result := FByKeyMap.Count;
end;

function TAbstractBidiMap<TKey, TValue>.GetEnumerator: IEnumerator<TKeyValuePair<TKey, TValue>>;
begin
  { Pass the enumerator from the key map }
  Result := FByKeyMap.GetEnumerator();
end;

function TAbstractBidiMap<TKey, TValue>.GetKeyList(const AValue: TValue): IEnexCollection<TKey>;
begin
  Result := FByValueMap[AValue];
end;

function TAbstractBidiMap<TKey, TValue>.GetValueList(const AKey: TKey): IEnexCollection<TValue>;
begin
  Result := FByKeyMap[AKey];
end;

function TAbstractBidiMap<TKey, TValue>.KeyHasValue(const AKey: TKey; const AValue: TValue): Boolean;
begin
  Result := ContainsPair(AKey, AValue);
end;

procedure TAbstractBidiMap<TKey, TValue>.Remove(const AKey: TKey; const AValue: TValue);
var
  LValues: IEnexCollection<TValue>;
  LValue: TValue;
begin
  { Check whether there is such a key }
  if not FByKeyMap.ContainsValue(AKey, AValue) then
    Exit;

  { Remove the stuff }
  FByKeyMap.Remove(AKey, AValue);
  FByValueMap.Remove(AValue, AKey);
end;

procedure TAbstractBidiMap<TKey, TValue>.Remove(const APair: TKeyValuePair<TKey, TValue>);
begin
  Remove(APair.Key, APair.Value);
end;

procedure TAbstractBidiMap<TKey, TValue>.Remove(const AKey: TKey);
begin
  RemoveKey(AKey);
end;

procedure TAbstractBidiMap<TKey, TValue>.RemoveKey(const AKey: TKey);
var
  LValues: IEnexCollection<TValue>;
  LValue: TValue;
begin
  { Check whether there is such a key }
  if not FByKeyMap.TryGetValues(AKey, LValues) then
    Exit;

  { Exclude the key for all values too }
  for LValue in LValues do
    FByValueMap.Remove(LValue, AKey);

  { And finally remove the key }
  FByKeyMap.Remove(AKey);
end;

procedure TAbstractBidiMap<TKey, TValue>.RemoveValue(const AValue: TValue);
var
  LKeys: IEnexCollection<TKey>;
  LValue: TKey;
begin
  { Check whether there is such a key }
  if not FByValueMap.TryGetValues(AValue, LKeys) then
    Exit;

  { Exclude the key for all values too}
  for LValue in LKeys do
    FByKeyMap.Remove(LValue, AValue);

  { And finally remove the key }
  FByValueMap.Remove(AValue);

//  { Cleanup the value if necessary }
//  if ValueType.Management = tmManual then
//    ValueType.Cleanup(LValue);
end;

function TAbstractBidiMap<TKey, TValue>.SelectKeys: IEnexCollection<TKey>;
begin
  { Pass the values on }
  Result := Keys;
end;

function TAbstractBidiMap<TKey, TValue>.SelectValues: IEnexCollection<TValue>;
begin
  { Pass the value on }
  Result := Values;
end;

function TAbstractBidiMap<TKey, TValue>.ValueForKey(const AKey: TKey): TValue;
begin
  Result := FByKeyMap[AKey].First;
end;

constructor TAbstractBidiMap<TKey, TValue>.Create(const AKeyType: IType<TKey>;
  const AValueType: IType<TValue>;
  const AArray: array of TKeyValuePair<TKey, TValue>);
var
  I: Integer;
begin
  { Call upper constructor }
  Create(AKeyType, AValueType);

  { Copy all items in }
  for I := 0 to Length(AArray) - 1 do
  begin
    Add(AArray[I]);
  end;
end;

constructor TAbstractBidiMap<TKey, TValue>.Create(const AKeyType: IType<TKey>; const AValueType: IType<TValue>);
var
  LKeyWrap: IType<TKey>;
  LValueWrap: IType<TValue>;
begin
  { Initialize instance }
  if (AKeyType = nil) then
     ExceptionHelper.Throw_ArgumentNilError('AKeyType');

  if (AValueType = nil) then
     ExceptionHelper.Throw_ArgumentNilError('AValueType');

  { Install the types }
  InstallTypes(AKeyType, AValueType);

  { Create type wrappers - basically disabling the cleanup for pne of the maps }
  LKeyWrap := TSuppressedWrapperType<TKey>.Create(KeyType);
  LValueWrap := TSuppressedWrapperType<TValue>.Create(ValueType);

  { Create the maps }
  FByKeyMap := CreateKeyMap(LKeyWrap, ValueType);
  FByValueMap := CreateValueMap(LValueWrap, KeyType);

  { The collections }
  FValueCollection := FByValueMap.Keys;
  FKeyCollection := FByKeyMap.Keys;
end;

constructor TAbstractBidiMap<TKey, TValue>.Create(const AKeyType: IType<TKey>;
  const AValueType: IType<TValue>;
  const AEnumerable: IEnumerable<TKeyValuePair<TKey, TValue>>);
var
  V: TKeyValuePair<TKey, TValue>;
begin
  { Call upper constructor }
  Create(AKeyType, AValueType);

  if (AEnumerable = nil) then
     ExceptionHelper.Throw_ArgumentNilError('AEnumerable');

  { Pump in all items }
  for V in AEnumerable do
  begin
{$IFNDEF BUG_GENERIC_INCOMPAT_TYPES}
    Add(V);
{$ELSE}
    Add(V.Key, V.Value);
{$ENDIF}
  end;
end;

{ TAbstractDistinctMultiMap<TKey, TValue>.TPairEnumerator }

constructor TAbstractDistinctMultiMap<TKey, TValue>.TPairEnumerator.Create(const ADict: TAbstractDistinctMultiMap<TKey, TValue>);
begin
  { Initialize }
  FDict := ADict;
  KeepObjectAlive(FDict);

  FVer := ADict.FVer;

  { Get the enumerator }
  FDictEnum := FDict.FDictionary.GetEnumerator();
end;

destructor TAbstractDistinctMultiMap<TKey, TValue>.TPairEnumerator.Destroy;
begin
  ReleaseObject(FDict);
  inherited;
end;

function TAbstractDistinctMultiMap<TKey, TValue>.TPairEnumerator.GetCurrent: TKeyValuePair<TKey,TValue>;
begin
  if FVer <> FDict.FVer then
     ExceptionHelper.Throw_CollectionChangedError();

  Result := FValue;
end;

function TAbstractDistinctMultiMap<TKey, TValue>.TPairEnumerator.MoveNext: Boolean;
begin
  { Repeat until something happens }
  while True do
  begin
    if FVer <> FDict.FVer then
       ExceptionHelper.Throw_CollectionChangedError();

    { We're still in the same KV? }
    if (FSetEnum <> nil) and (FSetEnum.MoveNext) then
    begin
      { Next element }
      FValue := TKeyValuePair<TKey, TValue>.Create(FDictEnum.Current.Key, FSetEnum.Current);
      Result := true;
      Exit;
    end;

    { Get the next KV pair from the dictionary }
    Result := FDictEnum.MoveNext();
    if not Result then
      Exit;

    { Reset the list }
    FSet := FDictEnum.Current.Value;
    FSetEnum := FSet.GetEnumerator();
  end;
end;

{ TAbstractDistinctMultiMap<TKey, TValue>.TKeyEnumerator }

constructor TAbstractDistinctMultiMap<TKey, TValue>.TKeyEnumerator.Create(const ADict: TAbstractDistinctMultiMap<TKey, TValue>);
begin
  { Initialize }
  FDict := ADict;
  KeepObjectAlive(FDict);

  FVer := ADict.FVer;
  FValue := default(TKey);

  { Create enumerator }
  FDictEnum := FDict.FDictionary.Keys.GetEnumerator();
end;

destructor TAbstractDistinctMultiMap<TKey, TValue>.TKeyEnumerator.Destroy;
begin
  ReleaseObject(FDict);
  inherited;
end;

function TAbstractDistinctMultiMap<TKey, TValue>.TKeyEnumerator.GetCurrent: TKey;
begin
  if FVer <> FDict.FVer then
     ExceptionHelper.Throw_CollectionChangedError();

  Result := FValue;
end;

function TAbstractDistinctMultiMap<TKey, TValue>.TKeyEnumerator.MoveNext: Boolean;
begin
  if FVer <> FDict.FVer then
     ExceptionHelper.Throw_CollectionChangedError();

  { Move next and get the value }
  Result := FDictEnum.MoveNext();
  if Result then
    FValue := FDictEnum.Current;
end;


{ TAbstractDistinctMultiMap<TKey, TValue>.TValueEnumerator }

constructor TAbstractDistinctMultiMap<TKey, TValue>.TValueEnumerator.Create(const ADict: TAbstractDistinctMultiMap<TKey, TValue>);
begin
  { Initialize }
  FDict := ADict;
  KeepObjectAlive(FDict);
  FVer := ADict.FVer;

  { Get the enumerator }
  FDictEnum := FDict.FDictionary.Values.GetEnumerator();
end;

destructor TAbstractDistinctMultiMap<TKey, TValue>.TValueEnumerator.Destroy;
begin
  ReleaseObject(FDict);
  inherited;
end;

function TAbstractDistinctMultiMap<TKey, TValue>.TValueEnumerator.GetCurrent: TValue;
begin
  if FVer <> FDict.FVer then
     ExceptionHelper.Throw_CollectionChangedError();

  Result := FValue;
end;

function TAbstractDistinctMultiMap<TKey, TValue>.TValueEnumerator.MoveNext: Boolean;
begin
  { Repeat until something happens }
  while True do
  begin
    if FVer <> FDict.FVer then
       ExceptionHelper.Throw_CollectionChangedError();

    { We're still in the same KV? }
    if (FSetEnum <> nil) and (FSetEnum.MoveNext()) then
    begin
      { Next element }
      FValue := FSetEnum.Current;

      Result := true;
      Exit;
    end;

    { Get the next KV pair from the dictionary }
    Result := FDictEnum.MoveNext();
    if not Result then
      Exit;

    { Reset the list }
    FSet := FDictEnum.Current;
    FSetEnum := FSet.GetEnumerator();
  end;
end;

{ TAbstractDistinctMultiMap<TKey, TValue>.TKeyCollection }

constructor TAbstractDistinctMultiMap<TKey, TValue>.TKeyCollection.Create(const ADict: TAbstractDistinctMultiMap<TKey, TValue>);
begin
  { Initialize }
  FDict := ADict;

  InstallType(ADict.KeyType);
end;

destructor TAbstractDistinctMultiMap<TKey, TValue>.TKeyCollection.Destroy;
begin
  inherited;
end;

function TAbstractDistinctMultiMap<TKey, TValue>.TKeyCollection.Empty: Boolean;
begin
  Result := (FDict.FDictionary.Count = 0);
end;

function TAbstractDistinctMultiMap<TKey, TValue>.TKeyCollection.GetCount: Cardinal;
begin
  { Number of elements is the same as key }
  Result := FDict.FDictionary.Count;
end;

function TAbstractDistinctMultiMap<TKey, TValue>.TKeyCollection.GetEnumerator: IEnumerator<TKey>;
begin
  Result := TKeyEnumerator.Create(Self.FDict);
end;

procedure TAbstractDistinctMultiMap<TKey, TValue>.TKeyCollection.CopyTo(var AArray: array of TKey);
begin
  { Call more generic function }
  CopyTo(AArray, 0);
end;

procedure TAbstractDistinctMultiMap<TKey, TValue>.TKeyCollection.CopyTo(var AArray: array of TKey; const StartIndex: Cardinal);
begin
  { Check for indexes }
  if StartIndex >= Cardinal(Length(AArray)) then
    ExceptionHelper.Throw_ArgumentOutOfRangeError('StartIndex');

  if (Cardinal(Length(AArray)) - StartIndex) < FDict.FDictionary.Count then
     ExceptionHelper.Throw_ArgumentOutOfSpaceError('AArray');

  { Simply copy using the dictionary provided methods }
  FDict.FDictionary.Keys.CopyTo(AArray, StartIndex);
end;

{ TAbstractDistinctMultiMap<TKey, TValue>.TValueCollection }

constructor TAbstractDistinctMultiMap<TKey, TValue>.TValueCollection.Create(const ADict: TAbstractDistinctMultiMap<TKey, TValue>);
begin
  { Initialize }
  FDict := ADict;

  InstallType(ADict.ValueType);
end;

destructor TAbstractDistinctMultiMap<TKey, TValue>.TValueCollection.Destroy;
begin
  inherited;
end;

function TAbstractDistinctMultiMap<TKey, TValue>.TValueCollection.Empty: Boolean;
begin
  Result := (FDict.FDictionary.Count = 0);
end;

function TAbstractDistinctMultiMap<TKey, TValue>.TValueCollection.GetCount: Cardinal;
begin
  { Number of elements is different use the count provided by the dictionary }
  Result := FDict.Count;
end;

function TAbstractDistinctMultiMap<TKey, TValue>.TValueCollection.GetEnumerator: IEnumerator<TValue>;
begin
  Result := TValueEnumerator.Create(Self.FDict);
end;

procedure TAbstractDistinctMultiMap<TKey, TValue>.TValueCollection.CopyTo(var AArray: array of TValue);
begin
  { Call more generic function }
  CopyTo(AArray, 0);
end;

procedure TAbstractDistinctMultiMap<TKey, TValue>.TValueCollection.CopyTo(var AArray: array of TValue; const StartIndex: Cardinal);
var
  LSet: ISet<TValue>;
  Value: TValue;
  X: Cardinal;
begin
  { Check for indexes }
  if StartIndex >= Cardinal(Length(AArray)) then
    ExceptionHelper.Throw_ArgumentOutOfRangeError('StartIndex');

  if (Cardinal(Length(AArray)) - StartIndex) < FDict.Count then
     ExceptionHelper.Throw_ArgumentOutOfSpaceError('AArray');

  X := StartIndex;

  { Iterate over all lists and copy thtm to array }
  for LSet in FDict.FDictionary.Values do
  begin
    for Value in LSet do
    begin
      AArray[X] := Value;
      Inc(X);
    end;
  end;
end;

{ TAbstractBag<T> }

procedure TAbstractBag<T>.Add(const AnItem: T; const Count: Cardinal);
var
  OldCount: Cardinal;
begin
  { Check count > 0 }
  if Count = 0 then
    Exit;

  { Add or update count }
  if FDictionary.TryGetValue(AnItem, OldCount) then
    FDictionary[AnItem] := OldCount + Count
  else
    FDictionary.Add(AnItem, Count);

  Inc(FKnownCount, Count);
  Inc(FVer);
end;

function TAbstractBag<T>.All(const APredicate: TFunc<T, Boolean>): Boolean;
begin
  { Use TDictionary's Keys }
  Result := FDictionary.Keys.All(APredicate);
end;

function TAbstractBag<T>.Any(const APredicate: TFunc<T, Boolean>): Boolean;
begin
  { Use TDictionary's Keys }
  Result := FDictionary.Keys.Any(APredicate);
end;

procedure TAbstractBag<T>.Clear;
begin
  if FDictionary <> nil then
  begin
    { Simply clear the dictionary }
    FDictionary.Clear();

    FKnownCount := 0;
    Inc(FVer);
  end;
end;

function TAbstractBag<T>.Contains(const AnItem: T; const Count: Cardinal): Boolean;
var
  InCount: Cardinal;
begin
  { Check count > 0 }
  if Count = 0 then
    Exit(true);

  { Check the counts in the bag }
  Result := (FDictionary.TryGetValue(AnItem, InCount)) and (InCount >= Count);
end;

procedure TAbstractBag<T>.CopyTo(var AArray: array of T);
begin
  { Call upper version }
  CopyTo(AArray, 0);
end;

procedure TAbstractBag<T>.CopyTo(var AArray: array of T; const StartIndex: Cardinal);
var
  TempArray: array of TKeyValuePair<T, Cardinal>;
  I, X, Y: Cardinal;
begin
  if StartIndex >= Cardinal(Length(AArray)) then
    ExceptionHelper.Throw_ArgumentOutOfRangeError('StartIndex');

  { Check for indexes }
  if (Cardinal(Length(AArray)) - StartIndex) < Count then
    ExceptionHelper.Throw_ArgumentOutOfSpaceError('AArray');

  { Nothing to do? }
  if Count = 0 then
    Exit;

  { Initialize the temporary array }
  SetLength(TempArray, FDictionary.Count);
  FDictionary.CopyTo(TempArray);

  X := StartIndex;

  { OK! Now let's simply copy }
  for I := 0 to Length(TempArray) - 1 do
  begin
    { Copy one value for a number of counts }
    for Y := 0 to TempArray[I].Value - 1 do
    begin
      AArray[X] := TempArray[I].Key;
      Inc(X);
    end;
  end;
end;

constructor TAbstractBag<T>.Create(const AArray: TFixedArray<T>);
begin
  { Call upper constructor }
  Create(TType<T>.Default, AArray);
end;

constructor TAbstractBag<T>.Create(const AArray: TDynamicArray<T>);
begin
  { Call upper constructor }
  Create(TType<T>.Default, AArray);
end;

constructor TAbstractBag<T>.Create(const AType: IType<T>; const AArray: TFixedArray<T>);
var
  I: Cardinal;
begin
  { Call upper constructor }
  Create(AType);

  { Copy all items in }
  if AArray.Length > 0 then
    for I := 0 to AArray.Length - 1 do
    begin
      Add(AArray[I]);
    end;
end;

constructor TAbstractBag<T>.Create(const AType: IType<T>; const AArray: TDynamicArray<T>);
var
  I: Cardinal;
begin
  { Call upper constructor }
  Create(AType);

  { Copy all items in }
  if AArray.Length > 0 then
    for I := 0 to AArray.Length - 1 do
    begin
      Add(AArray[I]);
    end;
end;

constructor TAbstractBag<T>.Create();
begin
  { Call upper constructor }
  Create(TType<T>.Default);
end;

constructor TAbstractBag<T>.Create(const AEnumerable: IEnumerable<T>);
begin
  { Call upper constructor }
  Create(TType<T>.Default, AEnumerable);
end;

constructor TAbstractBag<T>.Create(const AType: IType<T>; const AEnumerable: IEnumerable<T>);
var
  V: T;
begin
  if (AEnumerable = nil) then
     ExceptionHelper.Throw_ArgumentNilError('AEnumerable');

  { Call upper constructor }
  Create(AType);

  { Iterate and add }
  for V in AEnumerable do
    Add(V);
end;

constructor TAbstractBag<T>.Create(const AType: IType<T>; const AArray: array of T);
var
  I: Integer;
begin
  { Call upper constructor }
  Create(AType);

  { Copy all items in }
  for I := 0 to Length(AArray) - 1 do
  begin
    Add(AArray[I]);
  end;
end;

constructor TAbstractBag<T>.Create(const AType: IType<T>);
begin
  { Initialize instance }
  if (AType = nil) then
     ExceptionHelper.Throw_ArgumentNilError('AType');

  InstallType(AType);
  FDictionary := CreateDictionary(ElementType);

  FVer := 0;
  FKnownCount := 0;
end;

constructor TAbstractBag<T>.Create(const AArray: array of T);
begin
  { Call upper constructor }
  Create(TType<T>.Default, AArray);
end;

destructor TAbstractBag<T>.Destroy;
begin
  { Clear the bag first }
  Clear();

  inherited;
end;

function TAbstractBag<T>.Empty: Boolean;
begin
  Result := (FKnownCount = 0);
end;

function TAbstractBag<T>.First: T;
begin
  { Use TDictionary's Keys }
  Result := FDictionary.Keys.First();
end;

function TAbstractBag<T>.FirstOrDefault(const ADefault: T): T;
begin
  { Use TDictionary's Keys }
  Result := FDictionary.Keys.FirstOrDefault(ADefault);
end;

function TAbstractBag<T>.Last: T;
begin
  { Use TDictionary's Keys }
  Result := FDictionary.Keys.Last();
end;

function TAbstractBag<T>.LastOrDefault(const ADefault: T): T;
begin
  { Use TDictionary's Keys }
  Result := FDictionary.Keys.LastOrDefault(ADefault);
end;

function TAbstractBag<T>.Max: T;
begin
  { Use TDictionary's Keys }
  Result := FDictionary.Keys.Max();
end;

function TAbstractBag<T>.Min: T;
begin
  { Use TDictionary's Keys }
  Result := FDictionary.Keys.Min();
end;

function TAbstractBag<T>.GetCount: Cardinal;
begin
  { Dictionary knows the real count }
  Result := FKnownCount;
end;

function TAbstractBag<T>.GetItemCount(const AnItem: T): Cardinal;
begin
  { Get the count }
  if not FDictionary.TryGetValue(AnItem, Result) then
     Result := 0;
end;

function TAbstractBag<T>.GetEnumerator: IEnumerator<T>;
begin
  Result := TEnumerator.Create(Self);
end;

procedure TAbstractBag<T>.Remove(const AnItem: T; const Count: Cardinal);
var
  OldCount: Cardinal;
begin
  { Check count > 0 }
  if Count = 0 then
    Exit;

  { Check that the key os present in the dictionary first }
  if not FDictionary.TryGetValue(AnItem, OldCount) then
    Exit;

  if OldCount < Count then
    OldCount := 0
  else
    OldCount := OldCount - Count;

  { Update the counts }
  if OldCount = 0 then
    FDictionary.Remove(AnItem)
  else
    FDictionary[AnItem] := OldCount;

  Dec(FKnownCount, Count);
  Inc(FVer);
end;

procedure TAbstractBag<T>.RemoveAll(const AnItem: T);
var
  OldCount: Cardinal;
begin
  { Check that the key is present in the dictionary first }
  if not FDictionary.TryGetValue(AnItem, OldCount) then
    Exit;

  FDictionary.Remove(AnItem);

  Dec(FKnownCount, OldCount);
  Inc(FVer);
end;

procedure TAbstractBag<T>.SetItemCount(const AnItem: T; const Value: Cardinal);
var
  OldValue: Cardinal;
begin
  { Check count > 0 }
  if Count = 0 then
    Exit;

  if FDictionary.ContainsKey(AnItem) then
  begin
    OldValue := FDictionary[AnItem];
    FDictionary[AnItem] := Value;
  end else
  begin
    OldValue := 0;
    FDictionary.Add(AnItem, Value);
  end;

  { Change the counts }
  FKnownCount := FKnownCount - OldValue + Value;
  Inc(FVer);
end;

function TAbstractBag<T>.Single: T;
begin
  { Use TDictionary's Keys }
  Result := FDictionary.Keys.Single();
end;

function TAbstractBag<T>.SingleOrDefault(const ADefault: T): T;
begin
  { Use TDictionary's Keys }
  Result := FDictionary.Keys.SingleOrDefault(ADefault);
end;

{ TAbstractBag<T>.TEnumerator }

constructor TAbstractBag<T>.TEnumerator.Create(const ADict: TAbstractBag<T>);
begin
  { Initialize }
  FDict := ADict;
  KeepObjectAlive(FDict);

  FCurrentKV := FDict.FDictionary.GetEnumerator();

  FCurrentCount := 0;
  FValue := Default(T);

  FVer := ADict.FVer;
end;

destructor TAbstractBag<T>.TEnumerator.Destroy;
begin
  ReleaseObject(FDict);

  inherited;
end;

function TAbstractBag<T>.TEnumerator.GetCurrent: T;
begin
  if FVer <> FDict.FVer then
     ExceptionHelper.Throw_CollectionChangedError();

  Result := FValue;
end;

function TAbstractBag<T>.TEnumerator.MoveNext: Boolean;
begin
  { Repeat until something happens }
  while True do
  begin
    if FVer <> FDict.FVer then
       ExceptionHelper.Throw_CollectionChangedError();

    { We're still in the same KV? }
    if FCurrentCount <> 0 then
    begin
      { Decrease the count of the bag item }
      Dec(FCurrentCount);
      Result := true;

      Exit;
    end;

    { Get the next KV pair from the dictionary }
    Result := FCurrentKV.MoveNext();
    if not Result then
      Exit;

    { Copy the key/value }
    FCurrentCount := FCurrentKV.Current.Value;
    FValue := FCurrentKV.Current.Key;
  end;
end;


end.
