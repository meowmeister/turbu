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
unit DeHL.Collections.Base;
interface
uses
  SysUtils,
  DeHL.Base,
  DeHL.StrConsts,
  DeHL.Arrays,
  DeHL.Exceptions,
  DeHL.Types,
  DeHL.Serialization,
  DeHL.Converter,
  DeHL.KeyValuePair;

{
  *******
  ******* BASE INTERFACES
  *******
}
type
  { Enumerator }
  IEnumerator<T> = interface
    function GetCurrent(): T;
    function MoveNext(): Boolean;

    property Current: T read GetCurrent;
  end;

  { Enumerable }
  IEnumerable<T> = interface
    { Implement to generate enumerations }
    function GetEnumerator(): IEnumerator<T>;
  end;

  { Collection basics }
  ICollection<T> = interface(IEnumerable<T>)
    { Implement to support count of elements }
    function GetCount(): Cardinal;
    function Empty(): Boolean;

    { Gets the single element }
    function Single(): T;
    function SingleOrDefault(const ADefault: T): T;

    { Implement to support copy }
    procedure CopyTo(var AArray: array of T); overload;
    procedure CopyTo(var AArray: array of T; const StartIndex: Cardinal); overload;

    { To Array variants }
    function ToArray(): TArray<T>;
    function ToFixedArray(): TFixedArray<T>;
    function ToDynamicArray(): TDynamicArray<T>;

    property Count: Cardinal read GetCount;
  end;

  { Pre-declarations }
  IList<T> = interface;
  ISet<T> = interface;
  IDictionary<TKey, TValue> = interface;
  IEnexCollection<T> = interface;


  { Hack-record to allow generic methods in interfaces }
  TEnexExtOps<T> = record
  private
    FInstance: Pointer;
    FKeepAlive: IInterface;

  public
    function Select<TOut>(const ASelector: TFunc<T, TOut>; const AType: IType<TOut>): IEnexCollection<TOut>; overload;
    function Select<TOut>(const ASelector: TFunc<T, TOut>): IEnexCollection<TOut>; overload;
    function Cast<TOut>(const AType: IType<TOut>): IEnexCollection<TOut>; overload;
    function Cast<TOut>(): IEnexCollection<TOut>; overload;
  end;

  { Extended Enumerable }
  IEnexCollection<T> = interface(ICollection<T>)
    { Equality }
    function EqualsTo(const AEnumerable: IEnumerable<T>): Boolean;

    { Simple To methods }
    function ToList(): IList<T>;
    function ToSet(): ISet<T>;

    { Enex: T-returning methods }
    function Max(): T;
    function Min(): T;
    function First(): T;
    function FirstOrDefault(const ADefault: T): T;
    function Last(): T;
    function LastOrDefault(const ADefault: T): T;
    function Aggregate(const AAggregator: TFunc<T, T, T>): T;
    function AggregateOrDefault(const AAggregator: TFunc<T, T, T>; const ADefault: T): T;
    function ElementAt(const Index: Cardinal): T;
    function ElementAtOrDefault(const Index: Cardinal; const ADefault: T): T;

    { Enex: Predicates }
    function Any(const APredicate: TFunc<T, Boolean>): Boolean;
    function All(const APredicate: TFunc<T, Boolean>): Boolean;

    { Enex: Queries and related }
    function Where(const APredicate: TFunc<T, Boolean>): IEnexCollection<T>;
    function WhereLower(const ABound: T): IEnexCollection<T>;
    function WhereLowerOrEqual(const ABound: T): IEnexCollection<T>;
    function WhereGreater(const ABound: T): IEnexCollection<T>;
    function WhereGreaterOrEqual(const ABound: T): IEnexCollection<T>;
    function WhereBetween(const ALower, AHigher: T): IEnexCollection<T>;

    { Other operations }
    function Distinct(): IEnexCollection<T>;
    function Sorted(const Ascending: Boolean = true): IEnexCollection<T>;
    function Reversed(): IEnexCollection<T>;
    function Concat(const AEnumerable: IEnumerable<T>): IEnexCollection<T>;
    function Union(const AEnumerable: IEnumerable<T>): IEnexCollection<T>;
    function Exclude(const AEnumerable: IEnumerable<T>): IEnexCollection<T>;
    function Intersect(const AEnumerable: IEnumerable<T>): IEnexCollection<T>;
    function Range(const AStart, AEnd: Cardinal): IEnexCollection<T>;

    function Take(const ACount: Cardinal): IEnexCollection<T>;
    function TakeWhile(const APredicate: TFunc<T, Boolean>): IEnexCollection<T>;
    function TakeWhileLower(const ABound: T): IEnexCollection<T>;
    function TakeWhileLowerOrEqual(const ABound: T): IEnexCollection<T>;
    function TakeWhileGreater(const ABound: T): IEnexCollection<T>;
    function TakeWhileGreaterOrEqual(const ABound: T): IEnexCollection<T>;
    function TakeWhileBetween(const ALower, AHigher: T): IEnexCollection<T>;

    function Skip(const ACount: Cardinal): IEnexCollection<T>;
    function SkipWhile(const APredicate: TFunc<T, Boolean>): IEnexCollection<T>;
    function SkipWhileLower(const ABound: T): IEnexCollection<T>;
    function SkipWhileLowerOrEqual(const ABound: T): IEnexCollection<T>;
    function SkipWhileGreater(const ABound: T): IEnexCollection<T>;
    function SkipWhileGreaterOrEqual(const ABound: T): IEnexCollection<T>;
    function SkipWhileBetween(const ALower, AHigher: T): IEnexCollection<T>;

    { Extended operations }
    function Op: TEnexExtOps<T>;
  end;

  IEnexIndexedCollection<T> = interface(IEnexCollection<T>)
    { The access property }
    function GetItem(const Index: Cardinal): T;
    property Items[const Index: Cardinal]: T read GetItem; default;
  end;

  { Extendede Enumerable for Asscociative collections }
  IEnexAssociativeCollection<TKey, TValue> = interface(ICollection<TKeyValuePair<TKey, TValue>>)
    { Simple To methods }
    function ToDictionary(): IDictionary<TKey, TValue>;

    { Key/Value pair methods }
    function ValueForKey(const AKey: TKey): TValue;
    function KeyHasValue(const AKey: TKey; const AValue: TValue): Boolean;

    { Simple methods }
    function MaxKey(): TKey;
    function MinKey(): TKey;
    function MaxValue(): TValue;
    function MinValue(): TValue;

    { Keys and values }
    function SelectKeys(): IEnexCollection<TKey>;
    function SelectValues(): IEnexCollection<TValue>;

    { Key and value properties }
    property Keys: IEnexCollection<TKey> read SelectKeys;
    property Values: IEnexCollection<TValue> read SelectValues;

    { Selectors }
    function DistinctByKeys(): IEnexAssociativeCollection<TKey, TValue>;
    function DistinctByValues(): IEnexAssociativeCollection<TKey, TValue>;

    { Others }
    function Includes(const AEnumerable: IEnumerable<TKeyValuePair<TKey, TValue>>): Boolean;

    { Queries }
    function Where(const APredicate: TFunc<TKey, TValue, Boolean>): IEnexAssociativeCollection<TKey, TValue>;

    function WhereKeyLower(const ABound: TKey): IEnexAssociativeCollection<TKey, TValue>;
    function WhereKeyLowerOrEqual(const ABound: TKey): IEnexAssociativeCollection<TKey, TValue>;
    function WhereKeyGreater(const ABound: TKey): IEnexAssociativeCollection<TKey, TValue>;
    function WhereKeyGreaterOrEqual(const ABound: TKey): IEnexAssociativeCollection<TKey, TValue>;
    function WhereKeyBetween(const ALower, AHigher: TKey): IEnexAssociativeCollection<TKey, TValue>;

    function WhereValueLower(const ABound: TValue): IEnexAssociativeCollection<TKey, TValue>;
    function WhereValueLowerOrEqual(const ABound: TValue): IEnexAssociativeCollection<TKey, TValue>;
    function WhereValueGreater(const ABound: TValue): IEnexAssociativeCollection<TKey, TValue>;
    function WhereValueGreaterOrEqual(const ABound: TValue): IEnexAssociativeCollection<TKey, TValue>;
    function WhereValueBetween(const ALower, AHigher: TValue): IEnexAssociativeCollection<TKey, TValue>;
  end;

  { Stack behaviour }
  IStack<T> = interface(IEnexCollection<T>)
    { Clears the instance }
    procedure Clear();

    { Push/ Pop/ Peek }
    procedure Push(const AValue: T);
    function Pop(): T;
    function Peek(): T;

    { Direct removal }
    procedure Remove(const AValue: T);

    { Look-up }
    function Contains(const AValue: T): Boolean;
  end;

  { Queue behaviour }
  IQueue<T> = interface(IEnexCollection<T>)

    {  Modification }
    procedure Clear();

    { Push/ Pop/ Peek }
    procedure Enqueue(const AValue: T);
    function Dequeue(): T;
    function Peek(): T;

    { Look-up }
    function Contains(const AValue: T): Boolean;
  end;

  { Priority Queue behaviour }
  IPriorityQueue<TPriority, TValue> = interface(IEnexAssociativeCollection<TPriority, TValue>)
    {  Modification }
    procedure Clear();

    { Push/ Pop/ Peek }
    procedure Enqueue(const AValue: TValue); overload;
    procedure Enqueue(const AValue: TValue; const APriority: TPriority); overload;

    function Dequeue(): TValue;
    function Peek(): TValue;

    { Look-up }
    function Contains(const AValue: TValue): Boolean;
  end;

  { Set behaviour }
  ISet<T> = interface(IEnexCollection<T>)
    { Clears the set }
    procedure Clear();

    { Add/Remove }
    procedure Add(const AValue: T);
    procedure Remove(const AValue: T);

    { Look-up }
    function Contains(const AValue: T): Boolean;
  end;

  { Bag behaviour }
  IBag<T> = interface(IEnexCollection<T>)
    {  Modification }
    procedure Clear();

    { Addition }
    procedure Add(const AnItem: T; const Count: Cardinal = 1);

    { Removal }
    procedure Remove(const AnItem: T; const Count: Cardinal = 1);
    procedure RemoveAll(const AnItem: T);

    { Lookup }
    function Contains(const AnItem: T; const Count: Cardinal = 1): Boolean;

    { For indexing }
    function GetItemCount(const AnItem: T): Cardinal;
    procedure SetItemCount(const AnItem: T; const Value: Cardinal);

    { Properties }
    property Counts[const AnItem: T]: Cardinal read GetItemCount write SetItemCount; default;
  end;

  { The map! }
  IMap<TKey, TValue> = interface(IEnexAssociativeCollection<TKey, TValue>)
    { Clearing }
    procedure Clear();

    { Adding }
{$IFNDEF BUG_GENERIC_INCOMPAT_TYPES}
    procedure Add(const APair: TKeyValuePair<TKey, TValue>); overload;
{$ENDIF}

    procedure Add(const AKey: TKey; const AValue: TValue); overload;

    { Removal }
    procedure Remove(const AKey: TKey);

    { Lookup }
    function ContainsKey(const AKey: TKey): Boolean;
    function ContainsValue(const AValue: TValue): Boolean;
  end;

  { Dictionaries }
  IDictionary<TKey, TValue> = interface(IMap<TKey, TValue>)
    function TryGetValue(const AKey: TKey; out FoundValue: TValue): Boolean;

    { Properties }
    function GetItem(const Key: TKey): TValue;
    procedure SetItem(const Key: TKey; const Value: TValue);

    property Items[const Key: TKey]: TValue read GetItem write SetItem; default;
  end;

  { Collection map }
  ICollectionMap<TKey, TValue> = interface(IMap<TKey, TValue>)

    procedure Remove(const AKey: TKey; const AValue: TValue); overload;

{$IFNDEF BUG_GENERIC_INCOMPAT_TYPES}
    procedure Remove(const APair: TKeyValuePair<TKey, TValue>); overload;
{$ENDIF}

    { Lookup }
    function ContainsValue(const AKey: TKey; const AValue: TValue): Boolean; overload;

{$IFNDEF BUG_GENERIC_INCOMPAT_TYPES}
    function ContainsValue(const APair: TKeyValuePair<TKey, TValue>): Boolean; overload;
{$ENDIF}
  end;

  { Bidi-Maps }
  IBidiMap<TKey, TValue> = interface(IMap<TKey, TValue>)
    { Proper removal methods }
    procedure RemoveKey(const AKey: TKey);
    procedure RemoveValue(const AValue: TValue);

    procedure Remove(const AKey: TKey; const AValue: TValue); overload;

{$IFNDEF BUG_GENERIC_INCOMPAT_TYPES}
    procedure Remove(const APair: TKeyValuePair<TKey, TValue>); overload;
{$ENDIF}

    { Lookup }
    function ContainsPair(const AKey: TKey; const AValue: TValue): Boolean; overload;

{$IFNDEF BUG_GENERIC_INCOMPAT_TYPES}
    function ContainsPair(const APair: TKeyValuePair<TKey, TValue>): Boolean; overload;
{$ENDIF}

    { Properties }
    function GetValueList(const AKey: TKey): IEnexCollection<TValue>;
    property ByKeys[const AKey: TKey]: IEnexCollection<TValue> read GetValueList;

    function GetKeyList(const AValue: TValue): IEnexCollection<TKey>;
    property ByValues[const AValue: TValue]: IEnexCollection<TKey> read GetKeyList;
  end;

  { Multi-Maps }
  IMultiMap<TKey, TValue> = interface(ICollectionMap<TKey, TValue>)
    { Properties }
    function GetItemList(const Key: TKey): IEnexIndexedCollection<TValue>;
    property Items[const Key: TKey]: IEnexIndexedCollection<TValue> read GetItemList; default;

    { Try to get the value list }
    function TryGetValues(const AKey: TKey; out AValueList: IEnexIndexedCollection<TValue>): Boolean;
  end;

  { Set-Maps }
  IDistinctMultiMap<TKey, TValue> = interface(ICollectionMap<TKey, TValue>)
    { Properties }
    function GetItemList(const Key: TKey): IEnexCollection<TValue>;
    property Items[const Key: TKey]: IEnexCollection<TValue> read GetItemList; default;

    { Try to get the value list }
    function TryGetValues(const AKey: TKey; out AValueSet: IEnexCollection<TValue>): Boolean;
  end;

  { Collection basics }
  IList<T> = interface(IEnexIndexedCollection<T>)
    { Clears the set }
    procedure Clear();

    { Add/Remove }
    procedure Add(const AValue: T); overload;
    procedure Add(const AEnumerable: IEnumerable<T>); overload;

    { Look-up }
    function Contains(const AValue: T): Boolean;

    { Removes an element at a specified position }
    procedure RemoveAt(const AtIndex: Cardinal);
    procedure Remove(const AValue: T);

    { Indexing }
    function IndexOf(const AValue: T; const StartIndex, Count: Cardinal): Integer; overload;
    function IndexOf(const AValue: T; const StartIndex: Cardinal): Integer; overload;
    function IndexOf(const AValue: T): Integer; overload;

    { Indexing - reverse }
    function LastIndexOf(const AValue: T; const StartIndex, Count: Cardinal): Integer; overload;
    function LastIndexOf(const AValue: T; const StartIndex: Cardinal): Integer; overload;
    function LastIndexOf(const AValue: T): Integer; overload;
  end;

  { Sorted List behaviour }
  ISortedList<T> = interface(IList<T>)
    { More lookup options }
    function Max(): T;
    function Min(): T;
  end;

  { Dynamic interface }
  IDynamic = interface
    { Gets the current capacity of the collection }
    function GetCapacity(): Cardinal;

    { Grow/Shrink }
    procedure Shrink();
    procedure Grow();

    { Reader property for GetCapacity }
    property Capacity: Cardinal read GetCapacity;
  end;


{
  *******
  ******* ENEX BASE CLASSES
  *******
}
type
  { Generic enumerator object }
  TEnumerator<T> = class abstract(TRefCountedObject, IEnumerator<T>)
    { ... }
    function GetCurrent(): T; virtual; abstract;
    function MoveNext(): Boolean; virtual; abstract;

    { ... }
    property Current: T read GetCurrent;
  end;

  { Base of bases! The collection class! }
  TCollection<T> = class abstract(TRefCountedObject, ISerializable, ICollection<T>, IEnumerable<T>)
  protected
    { Implement to support count of elements }
    function GetCount(): Cardinal; virtual;

    { Override to provide proper serialization }
    procedure StartSerializing(const AData: TSerializationData); virtual;
    procedure EndSerializing(const AData: TSerializationData); virtual;

    procedure StartDeserializing(const AData: TDeserializationData); virtual;
    procedure EndDeserializing(const AData: TDeserializationData); virtual;

    procedure Serialize(const AData: TSerializationData); virtual; abstract;
    procedure Deserialize(const AData: TDeserializationData); virtual; abstract;
  public
    { Checks whether a collection is empty }
    function Empty(): Boolean; virtual;

    { Returns teh single element if the case }
    function Single(): T; virtual;
    function SingleOrDefault(const ADefault: T): T; virtual;

    { Implement to support copy }
    procedure CopyTo(var AArray: array of T); overload; virtual;
    procedure CopyTo(var AArray: array of T; const StartIndex: Cardinal); overload; virtual;

    function ToArray(): TArray<T>; virtual;
    function ToFixedArray(): TFixedArray<T>; virtual;
    function ToDynamicArray(): TDynamicArray<T>; virtual;

    { IEnumerable<T> }
    function GetEnumerator(): IEnumerator<T>; virtual; abstract;

    { Count property}
    property Count: Cardinal read GetCount;
  end;

  { Base class for all Enex-enabled collections }
  TEnexCollection<T> = class abstract(TCollection<T>, IEnexCollection<T>)
  private
    FElementType: IType<T>;

  protected
    { The actual type - read only }
    property ElementType: IType<T> read FElementType;

    { Override in descendants to support proper stuff }
    procedure InstallType(const AType: IType<T>); virtual;

    { Override to provide proper serialization }
    procedure DeserializeElement(const AElement: T); virtual;

    procedure Serialize(const AData: TSerializationData); override;
    procedure Deserialize(const AData: TDeserializationData); override;
  public
    { Cleanup }
    class procedure CleanupTypes;

    { Enex: T-returning methods }
    function Max(): T; virtual;
    function Min(): T; virtual;
    function First(): T; virtual;
    function FirstOrDefault(const ADefault: T): T; virtual;
    function Last(): T; virtual;
    function LastOrDefault(const ADefault: T): T; virtual;

    function Aggregate(const AAggregator: TFunc<T, T, T>): T; virtual;
    function AggregateOrDefault(const AAggregator: TFunc<T, T, T>; const ADefault: T): T; virtual;

    function ElementAt(const Index: Cardinal): T; virtual;
    function ElementAtOrDefault(const Index: Cardinal; const ADefault: T): T; virtual;

    { Enex: Predicates }
    function Any(const APredicate: TFunc<T, Boolean>): Boolean; virtual;
    function All(const APredicate: TFunc<T, Boolean>): Boolean; virtual;
    function EqualsTo(const AEnumerable: IEnumerable<T>): Boolean; virtual;

    { Enex: Queries and related }
    function Where(const APredicate: TFunc<T, Boolean>): IEnexCollection<T>;
    function WhereLower(const ABound: T): IEnexCollection<T>;
    function WhereLowerOrEqual(const ABound: T): IEnexCollection<T>;
    function WhereGreater(const ABound: T): IEnexCollection<T>;
    function WhereGreaterOrEqual(const ABound: T): IEnexCollection<T>;
    function WhereBetween(const ALower, AHigher: T): IEnexCollection<T>;

    { Other operations }
    function Distinct(): IEnexCollection<T>; virtual;
    function Sorted(const Ascending: Boolean = true): IEnexCollection<T>; virtual;
    function Reversed(): IEnexCollection<T>; virtual;
    function Concat(const AEnumerable: IEnumerable<T>): IEnexCollection<T>;
    function Union(const AEnumerable: IEnumerable<T>): IEnexCollection<T>;
    function Exclude(const AEnumerable: IEnumerable<T>): IEnexCollection<T>;
    function Intersect(const AEnumerable: IEnumerable<T>): IEnexCollection<T>;
    function Range(const AStart, AEnd: Cardinal): IEnexCollection<T>;

    function Take(const ACount: Cardinal): IEnexCollection<T>;
    function TakeWhile(const APredicate: TFunc<T, Boolean>): IEnexCollection<T>;
    function TakeWhileLower(const ABound: T): IEnexCollection<T>;
    function TakeWhileLowerOrEqual(const ABound: T): IEnexCollection<T>;
    function TakeWhileGreater(const ABound: T): IEnexCollection<T>;
    function TakeWhileGreaterOrEqual(const ABound: T): IEnexCollection<T>;
    function TakeWhileBetween(const ALower, AHigher: T): IEnexCollection<T>;

    function Skip(const ACount: Cardinal): IEnexCollection<T>;
    function SkipWhile(const APredicate: TFunc<T, Boolean>): IEnexCollection<T>;
    function SkipWhileLower(const ABound: T): IEnexCollection<T>;
    function SkipWhileLowerOrEqual(const ABound: T): IEnexCollection<T>;
    function SkipWhileGreater(const ABound: T): IEnexCollection<T>;
    function SkipWhileGreaterOrEqual(const ABound: T): IEnexCollection<T>;
    function SkipWhileBetween(const ALower, AHigher: T): IEnexCollection<T>;

    { Extended operations }
    function Op: TEnexExtOps<T>;

    { Simple To methods }
    function ToList(): IList<T>;
    function ToSet(): ISet<T>;

    { Static operations }
    class function Fill(const AElement: T; const Count: Cardinal; const AType: IType<T>): IEnexCollection<T>; overload; static;
    class function Fill(const AElement: T; const Count: Cardinal): IEnexCollection<T>; overload; static;

    class function Interval(const AStart, AEnd, AIncrement: T; const AType: IType<T>): IEnexCollection<T> ; overload; static;
    class function Interval(const AStart, AEnd, AIncrement: T): IEnexCollection<T>; overload; static;
    class function Interval(const AStart, AEnd: T; const AType: IType<T>): IEnexCollection<T>; overload; static;
    class function Interval(const AStart, AEnd: T): IEnexCollection<T>; overload; static;
  end;

  { Base class for all Enex-enabled associative collections }
  TEnexAssociativeCollection<TKey, TValue> = class abstract(TCollection<TKeyValuePair<TKey, TValue>>,
      IEnexAssociativeCollection<TKey, TValue>)
  private
    FKeyType: IType<TKey>;
    FValueType: IType<TValue>;

  protected
    { The actual types - read only }
    property KeyType: IType<TKey> read FKeyType;
    property ValueType: IType<TValue> read FValueType;

    { Override in descendants to support proper stuff }
    procedure InstallTypes(const AKeyType: IType<TKey>; const AValueType: IType<TValue>); virtual;

    { Override to provide proper serialization }
    procedure DeserializePair(const AKey: TKey; const AValue: TValue); virtual;

    procedure Serialize(const AData: TSerializationData); override;
    procedure Deserialize(const AData: TDeserializationData); override;
  public
    { Cleanup }
    class procedure CleanupTypes;

    { Enex - associativity }
    function ValueForKey(const AKey: TKey): TValue; virtual;
    function KeyHasValue(const AKey: TKey; const AValue: TValue): Boolean; virtual;

    { Enex - Associative collection }
    function MaxKey(): TKey; virtual;
    function MinKey(): TKey; virtual;
    function MaxValue(): TValue; virtual;
    function MinValue(): TValue; virtual;

    { Enex - equality }
    function Includes(const AEnumerable: IEnumerable<TKeyValuePair<TKey, TValue>>): Boolean; virtual;

    { Enex - selectors }
    function SelectKeys(): IEnexCollection<TKey>; virtual;
    function SelectValues(): IEnexCollection<TValue>; virtual;

    function DistinctByKeys(): IEnexAssociativeCollection<TKey, TValue>;
    function DistinctByValues(): IEnexAssociativeCollection<TKey, TValue>;

    { Enex - Queries }
    function Where(const APredicate: TFunc<TKey, TValue, Boolean>): IEnexAssociativeCollection<TKey, TValue>;

    function WhereKeyLower(const ABound: TKey): IEnexAssociativeCollection<TKey, TValue>;
    function WhereKeyLowerOrEqual(const ABound: TKey): IEnexAssociativeCollection<TKey, TValue>;
    function WhereKeyGreater(const ABound: TKey): IEnexAssociativeCollection<TKey, TValue>;
    function WhereKeyGreaterOrEqual(const ABound: TKey): IEnexAssociativeCollection<TKey, TValue>;
    function WhereKeyBetween(const ALower, AHigher: TKey): IEnexAssociativeCollection<TKey, TValue>;

    function WhereValueLower(const ABound: TValue): IEnexAssociativeCollection<TKey, TValue>;
    function WhereValueLowerOrEqual(const ABound: TValue): IEnexAssociativeCollection<TKey, TValue>;
    function WhereValueGreater(const ABound: TValue): IEnexAssociativeCollection<TKey, TValue>;
    function WhereValueGreaterOrEqual(const ABound: TValue): IEnexAssociativeCollection<TKey, TValue>;
    function WhereValueBetween(const ALower, AHigher: TValue): IEnexAssociativeCollection<TKey, TValue>;

    { Simple To methods }
    function ToDictionary(): IDictionary<TKey, TValue>;
  end;


{
  *******
  ******* ENEX OPERATIONAL ENUMERABLES
  *******
}
type
  { The "Where" collection }
  TEnexWhereCollection<T> = class sealed(TEnexCollection<T>)
  private
  type
    { The "Where" enumerator }
    TEnumerator = class(TEnumerator<T>)
    private
      FEnum: TEnexWhereCollection<T>;
      FIter: IEnumerator<T>;

    public
      { Constructor }
      constructor Create(const AEnum: TEnexWhereCollection<T>);

      { Destructor }
      destructor Destroy(); override;

      function GetCurrent(): T; override;
      function MoveNext(): Boolean; override;
    end;

  var
    FDeleteEnum: Boolean;
    FEnum: TEnexCollection<T>;
    FPredicate: TFunc<T, Boolean>;

  public
    { Constructors }
    constructor Create(const AEnumerable: TEnexCollection<T>; const APredicate: TFunc<T, Boolean>); overload;
    constructor CreateIntf(const AEnumerable: IEnumerable<T>; const APredicate: TFunc<T, Boolean>; const AType: IType<T>); overload;

    { Destructor }
    destructor Destroy(); override;

    { IEnumerable<T> }
    function GetEnumerator(): IEnumerator<T>; override;
  end;

  { The "Select" collection }
  TEnexSelectCollection<T, TOut> = class sealed(TEnexCollection<TOut>, IEnexCollection<TOut>)
  private
  type
    { The "Select" enumerator }
    TEnumerator = class(TEnumerator<TOut>)
    private
      FEnum: TEnexSelectCollection<T, TOut>;
      FIter: IEnumerator<T>;
      FCurrent: TOut;

    public
      { Constructor }
      constructor Create(const AEnum: TEnexSelectCollection<T, TOut>);

      { Destructor }
      destructor Destroy(); override;

      function GetCurrent(): TOut; override;
      function MoveNext(): Boolean; override;
    end;

  var
    FDeleteEnum: Boolean;
    FEnum: TEnexCollection<T>;
    FSelector: TFunc<T, TOut>;

  protected
    { Enex: Defaults }
    function GetCount(): Cardinal; override;

  public
    { Constructors }
    constructor Create(const AEnumerable: TEnexCollection<T>; const ASelector: TFunc<T, TOut>; const AType: IType<TOut>); overload;
    constructor CreateIntf(const AEnumerable: IEnumerable<T>; const ASelector: TFunc<T, TOut>; const AType: IType<TOut>); overload;

    { Destructor }
    destructor Destroy(); override;

    { IEnumerable<T> }
    function GetEnumerator(): IEnumerator<TOut>; override;

    { Enex Overrides }
    function Empty(): Boolean; override;
    function First(): TOut; override;
    function Last(): TOut; override;
    function Single(): TOut; override;
    function ElementAt(const Index: Cardinal): TOut; override;
  end;

  { The "Cast" collection }
  TEnexCastCollection<T, TOut> = class sealed(TEnexCollection<TOut>, IEnexCollection<TOut>)
  private
  type
    { The "Cast" enumerator }
    TEnumerator = class(TEnumerator<TOut>)
    private
      FEnum: TEnexCastCollection<T, TOut>;
      FIter: IEnumerator<T>;
      FCurrent: TOut;

    public
      { Constructor }
      constructor Create(const AEnum: TEnexCastCollection<T, TOut>);

      { Destructor }
      destructor Destroy(); override;

      function GetCurrent(): TOut; override;
      function MoveNext(): Boolean; override;
    end;

  var
    FDeleteEnum: Boolean;
    FEnum: TEnexCollection<T>;
    FConverter: IConverter<T, TOut>;

  protected
    { Enex: Defaults }
    function GetCount(): Cardinal; override;

  public
    { Constructors }
    constructor Create(const AEnumerable: TEnexCollection<T>; const AOutType: IType<TOut>); overload;
    constructor CreateIntf(const AEnumerable: IEnumerable<T>; const AInType: IType<T>; const AOutType: IType<TOut>); overload;

    { Destructor }
    destructor Destroy(); override;

    { IEnumerable<T> }
    function GetEnumerator(): IEnumerator<TOut>; override;

    { Enex Overrides }
    function Empty(): Boolean; override;
    function First(): TOut; override;
    function Last(): TOut; override;
    function Single(): TOut; override;
    function ElementAt(const Index: Cardinal): TOut; override;
  end;

  { The "Concatenation" collection }
  TEnexConcatCollection<T> = class sealed(TEnexCollection<T>)
  private
  type
    { The "Concatenation" enumerator }
    TEnumerator = class(TEnumerator<T>)
    private
      FEnum: TEnexConcatCollection<T>;
      FIter1, FIter2: IEnumerator<T>;

    public
      { Constructor }
      constructor Create(const AEnum: TEnexConcatCollection<T>);

      { Destructor }
      destructor Destroy(); override;

      function GetCurrent(): T; override;
      function MoveNext(): Boolean; override;
    end;

  var
    FEnum1: TEnexCollection<T>;
    FEnum2: TEnexCollection<T>;
    FDeleteEnum1,
      FDeleteEnum2: Boolean;
  protected
    { ICollection support/hidden }
    function GetCount(): Cardinal; override;

  public
    { Constructors }
    constructor Create(const AEnumerable1: TEnexCollection<T>;
      const AEnumerable2: TEnexCollection<T>); overload;

    constructor CreateIntf(const AEnumerable1: IEnumerable<T>;
      const AEnumerable2: IEnumerable<T>; const AType: IType<T>); overload;

    constructor CreateIntf2(const AEnumerable1: TEnexCollection<T>;
      const AEnumerable2: IEnumerable<T>; const AType: IType<T>); overload;

    constructor CreateIntf1(const AEnumerable1: IEnumerable<T>;
      const AEnumerable2: TEnexCollection<T>; const AType: IType<T>); overload;

    { Destructor }
    destructor Destroy(); override;

    { IEnumerable<T> }
    function GetEnumerator(): IEnumerator<T>; override;

    { Enex Overrides }
    function Empty(): Boolean; override;
    function Any(const APredicate: TFunc<T, Boolean>): Boolean; override;
    function All(const APredicate: TFunc<T, Boolean>): Boolean; override;
  end;

  { The "Union" collection }
  TEnexUnionCollection<T> = class sealed(TEnexCollection<T>)
  private
  type
    { The "Union" enumerator }
    TEnumerator = class(TEnumerator<T>)
    private
      FEnum: TEnexUnionCollection<T>;
      FIter1, FIter2: IEnumerator<T>;
      FSet: ISet<T>;

    public
      { Constructor }
      constructor Create(const AEnum: TEnexUnionCollection<T>);

      { Destructor }
      destructor Destroy(); override;

      function GetCurrent(): T; override;
      function MoveNext(): Boolean; override;
    end;

  var
    FEnum1: TEnexCollection<T>;
    FEnum2: TEnexCollection<T>;
    FDeleteEnum1,
      FDeleteEnum2: Boolean;
  public
    { Constructors }
    constructor Create(const AEnumerable1: TEnexCollection<T>;
      const AEnumerable2: TEnexCollection<T>); overload;

    constructor CreateIntf(const AEnumerable1: IEnumerable<T>;
      const AEnumerable2: IEnumerable<T>; const AType: IType<T>); overload;

    constructor CreateIntf2(const AEnumerable1: TEnexCollection<T>;
      const AEnumerable2: IEnumerable<T>; const AType: IType<T>); overload;

    constructor CreateIntf1(const AEnumerable1: IEnumerable<T>;
      const AEnumerable2: TEnexCollection<T>; const AType: IType<T>); overload;

    { Destructor }
    destructor Destroy(); override;

    { IEnumerable<T> }
    function GetEnumerator(): IEnumerator<T>; override;
  end;

  { The "Exclusion" collection }
  TEnexExclusionCollection<T> = class sealed(TEnexCollection<T>)
  private
  type
    { The "Exclusion" enumerator }
    TEnumerator = class(TEnumerator<T>)
    private
      FEnum: TEnexExclusionCollection<T>;
      FIter: IEnumerator<T>;
      FSet: ISet<T>;

    public
      { Constructor }
      constructor Create(const AEnum: TEnexExclusionCollection<T>);

      { Destructor }
      destructor Destroy(); override;

      function GetCurrent(): T; override;
      function MoveNext(): Boolean; override;
    end;

  var
    FEnum1: TEnexCollection<T>;
    FEnum2: TEnexCollection<T>;
    FDeleteEnum1,
      FDeleteEnum2: Boolean;

  public
    { Constructors }
    constructor Create(const AEnumerable1: TEnexCollection<T>;
      const AEnumerable2: TEnexCollection<T>); overload;

    constructor CreateIntf(const AEnumerable1: IEnumerable<T>;
      const AEnumerable2: IEnumerable<T>; const AType: IType<T>); overload;

    constructor CreateIntf2(const AEnumerable1: TEnexCollection<T>;
      const AEnumerable2: IEnumerable<T>; const AType: IType<T>); overload;

    constructor CreateIntf1(const AEnumerable1: IEnumerable<T>;
      const AEnumerable2: TEnexCollection<T>; const AType: IType<T>); overload;

    { Destructor }
    destructor Destroy(); override;

    { IEnumerable<T> }
    function GetEnumerator(): IEnumerator<T>; override;
  end;

  { The "Intersection" collection }
  TEnexIntersectionCollection<T> = class sealed(TEnexCollection<T>)
  private
  type
    { The "Intersection" enumerator }
    TEnumerator = class(TEnumerator<T>)
    private
      FEnum: TEnexIntersectionCollection<T>;
      FIter: IEnumerator<T>;
      FSet: ISet<T>;

    public
      { Constructor }
      constructor Create(const AEnum: TEnexIntersectionCollection<T>);

      { Destructor }
      destructor Destroy(); override;

      function GetCurrent(): T; override;
      function MoveNext(): Boolean; override;
    end;

  var
    FEnum1: TEnexCollection<T>;
    FEnum2: TEnexCollection<T>;
    FDeleteEnum1,
      FDeleteEnum2: Boolean;

  public
    { Constructors }
    constructor Create(const AEnumerable1: TEnexCollection<T>;
      const AEnumerable2: TEnexCollection<T>); overload;

    constructor CreateIntf(const AEnumerable1: IEnumerable<T>;
      const AEnumerable2: IEnumerable<T>; const AType: IType<T>); overload;

    constructor CreateIntf2(const AEnumerable1: TEnexCollection<T>;
      const AEnumerable2: IEnumerable<T>; const AType: IType<T>); overload;

    constructor CreateIntf1(const AEnumerable1: IEnumerable<T>;
      const AEnumerable2: TEnexCollection<T>; const AType: IType<T>); overload;

    { Destructor }
    destructor Destroy(); override;

    { IEnumerable<T> }
    function GetEnumerator(): IEnumerator<T>; override;
  end;

  { The "Distinct" collection }
  TEnexDistinctCollection<T> = class sealed(TEnexCollection<T>)
  private
  type
    { The "Distinct" enumerator }
    TEnumerator = class(TEnumerator<T>)
    private
      FEnum: TEnexDistinctCollection<T>;
      FIter: IEnumerator<T>;
      FSet: ISet<T>;

    public
      { Constructor }
      constructor Create(const AEnum: TEnexDistinctCollection<T>);

      { Destructor }
      destructor Destroy(); override;

      function GetCurrent(): T; override;
      function MoveNext(): Boolean; override;
    end;

  var
    FEnum: TEnexCollection<T>;
    FDeleteEnum: Boolean;

  public
    { Constructors }
    constructor Create(const AEnumerable: TEnexCollection<T>); overload;
    constructor CreateIntf(const AEnumerable: IEnumerable<T>; const AType: IType<T>); overload;

    { Destructor }
    destructor Destroy(); override;

    { IEnumerable<T> }
    function GetEnumerator(): IEnumerator<T>; override;
  end;

  { The "Range" collection }
  TEnexRangeCollection<T> = class sealed(TEnexCollection<T>)
  private
  type
    { The "Range" enumerator }
    TEnumerator = class(TEnumerator<T>)
    private
      FEnum: TEnexRangeCollection<T>;
      FIter: IEnumerator<T>;
      FIdx: Cardinal;
    public
      { Constructor }
      constructor Create(const AEnum: TEnexRangeCollection<T>);

      { Destructor }
      destructor Destroy(); override;

      function GetCurrent(): T; override;
      function MoveNext(): Boolean; override;
    end;

  var
    FStart, FEnd: Cardinal;
    FEnum: TEnexCollection<T>;
    FDeleteEnum: Boolean;

  public
    { Constructors }
    constructor Create(const AEnumerable: TEnexCollection<T>; const AStart, AEnd: Cardinal); overload;
    constructor CreateIntf(const AEnumerable: IEnumerable<T>; const AStart, AEnd: Cardinal; const AType: IType<T>); overload;

    { Destructor }
    destructor Destroy(); override;

    { IEnumerable<T> }
    function GetEnumerator(): IEnumerator<T>; override;
  end;

  { The "Skip" collection }
  TEnexSkipCollection<T> = class sealed(TEnexCollection<T>)
  private
  type
    { The "Skip" enumerator }
    TEnumerator = class(TEnumerator<T>)
    private
      FEnum: TEnexSkipCollection<T>;
      FIter: IEnumerator<T>;
      FIdx: Cardinal;
    public
      { Constructor }
      constructor Create(const AEnum: TEnexSkipCollection<T>);

      { Destructor }
      destructor Destroy(); override;

      function GetCurrent(): T; override;
      function MoveNext(): Boolean; override;
    end;

  var
    FCount: Cardinal;
    FEnum: TEnexCollection<T>;
    FDeleteEnum: Boolean;

  public
    { Constructors }
    constructor Create(const AEnumerable: TEnexCollection<T>; const ACount: Cardinal); overload;
    constructor CreateIntf(const AEnumerable: IEnumerable<T>; const ACount: Cardinal; const AType: IType<T>); overload;

    { Destructor }
    destructor Destroy(); override;

    { IEnumerable<T> }
    function GetEnumerator(): IEnumerator<T>; override;
  end;

  { The "Take" collection }
  TEnexTakeCollection<T> = class sealed(TEnexCollection<T>)
  private
  type
    { The "Take" enumerator }
    TEnumerator = class(TEnumerator<T>)
    private
      FEnum: TEnexTakeCollection<T>;
      FIter: IEnumerator<T>;
      FIdx: Cardinal;

    public
      { Constructor }
      constructor Create(const AEnum: TEnexTakeCollection<T>);

      { Destructor }
      destructor Destroy(); override;

      function GetCurrent(): T; override;
      function MoveNext(): Boolean; override;
    end;

  var
    FCount: Cardinal;
    FEnum: TEnexCollection<T>;
    FDeleteEnum: Boolean;

  public
    { Constructors }
    constructor Create(const AEnumerable: TEnexCollection<T>; const ACount: Cardinal); overload;
    constructor CreateIntf(const AEnumerable: IEnumerable<T>; const ACount: Cardinal; const AType: IType<T>); overload;

    { Destructor }
    destructor Destroy(); override;

    { IEnumerable<T> }
    function GetEnumerator(): IEnumerator<T>; override;
  end;

  { The "Fill" collection }
  TEnexFillCollection<T> = class sealed(TEnexCollection<T>)
  private
  type
    { The "Fill" enumerator }
    TEnumerator = class(TEnumerator<T>)
    private
      FEnum: TEnexFillCollection<T>;
      FCount: Cardinal;
    public
      { Constructor }
      constructor Create(const AEnum: TEnexFillCollection<T>);

      { Destructor }
      destructor Destroy(); override;

      function GetCurrent(): T; override;
      function MoveNext(): Boolean; override;
    end;

  var
    FElement: T;
    FCount: Cardinal;

  protected
    { Enex: Defaults }
    function GetCount(): Cardinal; override;
  public
    { Constructors }
    constructor Create(const AElement: T; const Count: Cardinal; const AType: IType<T>);

    { IEnumerable<T> }
    function GetEnumerator(): IEnumerator<T>; override;

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
    function Aggregate(const AAggregator: TFunc<T, T, T>): T; override;
    function AggregateOrDefault(const AAggregator: TFunc<T, T, T>; const ADefault: T): T; override;
    function ElementAt(const Index: Cardinal): T; override;
    function ElementAtOrDefault(const Index: Cardinal; const ADefault: T): T; override;
    function Any(const APredicate: TFunc<T, Boolean>): Boolean; override;
    function All(const APredicate: TFunc<T, Boolean>): Boolean; override;
    function EqualsTo(const AEnumerable: IEnumerable<T>): Boolean; override;
  end;

  { The "Interval" collection }
  TEnexIntervalCollection<T> = class sealed(TEnexCollection<T>)
  private
  type
    { The "Interval" enumerator }
    TEnumerator = class(TEnumerator<T>)
    private
      FEnum: TEnexIntervalCollection<T>;
      FNow: T;
      FNowVariant: Variant;
    public
      { Constructor }
      constructor Create(const AEnum: TEnexIntervalCollection<T>);

      { Destructor }
      destructor Destroy(); override;

      function GetCurrent(): T; override;
      function MoveNext(): Boolean; override;
    end;

  var
    FLower, FHigher, FIncrement: T;

  public
    { Constructors }
    constructor Create(const ALower, AHigher, AIncrement: T; const AType: IType<T>);

    { IEnumerable<T> }
    function GetEnumerator(): IEnumerator<T>; override;

    { Enex Overrides }
    function Empty(): Boolean; override;
    function Min(): T; override;
    function First(): T; override;
    function FirstOrDefault(const ADefault: T): T; override;
  end;

  { The "Take While" collection }
  TEnexTakeWhileCollection<T> = class sealed(TEnexCollection<T>)
  private
  type
    { The "Take While" enumerator }
    TEnumerator = class(TEnumerator<T>)
    private
      FEnum: TEnexTakeWhileCollection<T>;
      FIter: IEnumerator<T>;

    public
      { Constructor }
      constructor Create(const AEnum: TEnexTakeWhileCollection<T>);

      { Destructor }
      destructor Destroy(); override;

      function GetCurrent(): T; override;
      function MoveNext(): Boolean; override;
    end;

  var
    FDeleteEnum: Boolean;
    FEnum: TEnexCollection<T>;
    FPredicate: TFunc<T, Boolean>;

  public
    { Constructors }
    constructor Create(const AEnumerable: TEnexCollection<T>; const APredicate: TFunc<T, Boolean>); overload;
    constructor CreateIntf(const AEnumerable: IEnumerable<T>; const APredicate: TFunc<T, Boolean>; const AType: IType<T>); overload;

    { Destructor }
    destructor Destroy(); override;

    { IEnumerable<T> }
    function GetEnumerator(): IEnumerator<T>; override;
  end;

  { The "Skip While" collection }
  TEnexSkipWhileCollection<T> = class sealed(TEnexCollection<T>)
  private
  type
    { The "Skip While" enumerator }
    TEnumerator = class(TEnumerator<T>)
    private
      FEnum: TEnexSkipWhileCollection<T>;
      FIter: IEnumerator<T>;
      FStop: Boolean;
    public
      { Constructor }
      constructor Create(const AEnum: TEnexSkipWhileCollection<T>);

      { Destructor }
      destructor Destroy(); override;

      function GetCurrent(): T; override;
      function MoveNext(): Boolean; override;
    end;

  var
    FDeleteEnum: Boolean;
    FEnum: TEnexCollection<T>;
    FPredicate: TFunc<T, Boolean>;

  public
    { Constructors }
    constructor Create(const AEnumerable: TEnexCollection<T>; const APredicate: TFunc<T, Boolean>); overload;
    constructor CreateIntf(const AEnumerable: IEnumerable<T>; const APredicate: TFunc<T, Boolean>; const AType: IType<T>); overload;

    { Destructor }
    destructor Destroy(); override;

    { IEnumerable<T> }
    function GetEnumerator(): IEnumerator<T>; override;
  end;

  { The "Wrap" collection }
  TEnexWrapCollection<T> = class sealed(TEnexCollection<T>)
  private
    FEnum: IEnumerable<T>;

  public
    { Constructors }
    constructor Create(const AEnumerable: IEnumerable<T>; const AType: IType<T>);

    { IEnumerable<T> }
    function GetEnumerator(): IEnumerator<T>; override;
  end;

  { The "Wrap associative" collection }
  TEnexAssociativeWrapCollection<TKey, TValue> = class sealed(TEnexAssociativeCollection<TKey, TValue>,
    IEnexAssociativeCollection<TKey, TValue>)
  private
    FEnum: IEnumerable<TKeyValuePair<TKey, TValue>>;

  public
    { Constructors }
    constructor Create(const AEnumerable: IEnumerable<TKeyValuePair<TKey, TValue>>; const AKeyType: IType<TKey>;
      const AValueType: IType<TValue>);

    { IEnumerable<T> }
    function GetEnumerator(): IEnumerator<TKeyValuePair<TKey, TValue>>; override;
  end;

  { The "Select Keys" collection }
  TEnexSelectKeysCollection<TKey, TValue> = class sealed(TEnexCollection<TKey>)
  private
  type
    { The "Select Keys" enumerator }
    TEnumerator = class(TEnumerator<TKey>)
    private
      FEnum: TEnexSelectKeysCollection<TKey, TValue>;
      FIter: IEnumerator<TKeyValuePair<TKey, TValue>>;
      FCurrent: TKey;

    public
      { Constructor }
      constructor Create(const AEnum: TEnexSelectKeysCollection<TKey, TValue>);

      { Destructor }
      destructor Destroy(); override;

      function GetCurrent(): TKey; override;
      function MoveNext(): Boolean; override;
    end;

  var
    FDeleteEnum: Boolean;
    FEnum: TEnexAssociativeCollection<TKey, TValue>;

  protected
    { Enex: Defaults }
    function GetCount(): Cardinal; override;
  public
    { Constructors }
    constructor Create(const AEnumerable: TEnexAssociativeCollection<TKey, TValue>); overload;
    constructor CreateIntf(const AEnumerable: IEnumerable<TKeyValuePair<TKey, TValue>>;
      const AKeyType: IType<TKey>; const AValueType: IType<TValue>); overload;

    { Destructor }
    destructor Destroy(); override;

    { IEnumerable<T> }
    function GetEnumerator(): IEnumerator<TKey>; override;
  end;

  { The "Select Values" collection }
  TEnexSelectValuesCollection<TKey, TValue> = class sealed(TEnexCollection<TValue>)
  private
  type
    { The "Select Keys" enumerator }
    TEnumerator = class(TEnumerator<TValue>)
    private
      FEnum: TEnexSelectValuesCollection<TKey, TValue>;
      FIter: IEnumerator<TKeyValuePair<TKey, TValue>>;
      FCurrent: TValue;

    public
      { Constructor }
      constructor Create(const AEnum: TEnexSelectValuesCollection<TKey, TValue>);

      { Destructor }
      destructor Destroy(); override;

      function GetCurrent(): TValue; override;
      function MoveNext(): Boolean; override;
    end;

  var
    FDeleteEnum: Boolean;
    FEnum: TEnexAssociativeCollection<TKey, TValue>;

  protected
    { Enex: Defaults }
    function GetCount(): Cardinal; override;
  public
    { Constructors }
    constructor Create(const AEnumerable: TEnexAssociativeCollection<TKey, TValue>); overload;
    constructor CreateIntf(const AEnumerable: IEnumerable<TKeyValuePair<TKey, TValue>>;
      const AKeyType: IType<TKey>; const AValueType: IType<TValue>); overload;

    { Destructor }
    destructor Destroy(); override;

    { IEnumerable<T> }
    function GetEnumerator(): IEnumerator<TValue>; override;
  end;

  { The "Where" associative collection }
  TEnexAssociativeWhereCollection<TKey, TValue> = class sealed(TEnexAssociativeCollection<TKey, TValue>,
      IEnexAssociativeCollection<TKey, TValue>)
  private
  type
    { The "Where" associative enumerator }
    TEnumerator = class(TEnumerator<TKeyValuePair<TKey, TValue>>)
    private
      FEnum: TEnexAssociativeWhereCollection<TKey, TValue>;
      FIter: IEnumerator<TKeyValuePair<TKey, TValue>>;

    public
      { Constructor }
      constructor Create(const AEnum: TEnexAssociativeWhereCollection<TKey, TValue>);

      { Destructor }
      destructor Destroy(); override;

      function GetCurrent(): TKeyValuePair<TKey, TValue>; override;
      function MoveNext(): Boolean; override;
    end;

  var
    FDeleteEnum: Boolean;
    FEnum: TEnexAssociativeCollection<TKey, TValue>;
    FPredicate: TFunc<TKey, TValue, Boolean>;

  public
    { Constructors }
    constructor Create(const AEnumerable: TEnexAssociativeCollection<TKey, TValue>;
        const APredicate: TFunc<TKey, TValue, Boolean>); overload;

    constructor CreateIntf(const AEnumerable: IEnumerable<TKeyValuePair<TKey, TValue>>;
      const APredicate: TFunc<TKey, TValue, Boolean>;
      const AKeyType: IType<TKey>; const AValueType: IType<TValue>); overload;

    { Destructor }
    destructor Destroy(); override;

    { IEnumerable<T> }
    function GetEnumerator(): IEnumerator<TKeyValuePair<TKey, TValue>>; override;
  end;

  { The "Distinct By Keys" associative collection }
  TEnexAssociativeDistinctByKeysCollection<TKey, TValue> = class sealed(TEnexAssociativeCollection<TKey, TValue>)
  private
  type
    { The "Distinct By Keys" associative enumerator }
    TEnumerator = class(TEnumerator<TKeyValuePair<TKey, TValue>>)
    private
      FEnum: TEnexAssociativeDistinctByKeysCollection<TKey, TValue>;
      FIter: IEnumerator<TKeyValuePair<TKey, TValue>>;
      FSet: ISet<TKey>;

    public
      { Constructor }
      constructor Create(const AEnum: TEnexAssociativeDistinctByKeysCollection<TKey, TValue>);

      { Destructor }
      destructor Destroy(); override;

      function GetCurrent(): TKeyValuePair<TKey, TValue>; override;
      function MoveNext(): Boolean; override;
    end;

  var
    FDeleteEnum: Boolean;
    FEnum: TEnexAssociativeCollection<TKey, TValue>;

  public
    { Constructors }
    constructor Create(const AEnumerable: TEnexAssociativeCollection<TKey, TValue>); overload;

    constructor CreateIntf(const AEnumerable: IEnumerable<TKeyValuePair<TKey, TValue>>; const AKeyType: IType<TKey>;
      const AValueType: IType<TValue>); overload;

    { Destructor }
    destructor Destroy(); override;

    { IEnumerable<T> }
    function GetEnumerator(): IEnumerator<TKeyValuePair<TKey, TValue>>; override;
  end;

  { The "Distinct By Values" associative collection }
  TEnexAssociativeDistinctByValuesCollection<TKey, TValue> = class sealed(TEnexAssociativeCollection<TKey, TValue>)
  private
  type
    { The "Distinct By Keys" associative enumerator }
    TEnumerator = class(TEnumerator<TKeyValuePair<TKey, TValue>>)
    private
      FEnum: TEnexAssociativeDistinctByValuesCollection<TKey, TValue>;
      FIter: IEnumerator<TKeyValuePair<TKey, TValue>>;
      FSet: ISet<TValue>;

    public
      { Constructor }
      constructor Create(const AEnum: TEnexAssociativeDistinctByValuesCollection<TKey, TValue>);

      { Destructor }
      destructor Destroy(); override;

      function GetCurrent(): TKeyValuePair<TKey, TValue>; override;
      function MoveNext(): Boolean; override;
    end;

  var
    FDeleteEnum: Boolean;
    FEnum: TEnexAssociativeCollection<TKey, TValue>;

  public
    { Constructors }
    constructor Create(const AEnumerable: TEnexAssociativeCollection<TKey, TValue>); overload;

    constructor CreateIntf(const AEnumerable: IEnumerable<TKeyValuePair<TKey, TValue>>; const AKeyType: IType<TKey>;
      const AValueType: IType<TValue>); overload;

    { Destructor }
    destructor Destroy(); override;

    { IEnumerable<T> }
    function GetEnumerator(): IEnumerator<TKeyValuePair<TKey, TValue>>; override;
  end;



implementation
uses
  DeHL.Collections.HashSet,
  DeHL.Collections.List,
  DeHL.Collections.Dictionary;

{ TEnexCollection<T> }

procedure TEnexCollection<T>.InstallType(const AType: IType<T>);
begin
  { Pass through }
  FElementType := AType;
end;

function TEnexCollection<T>.Aggregate(const AAggregator: TFunc<T, T, T>): T;
var
  Enum: IEnumerator<T>;
begin
  if not Assigned(AAggregator) then
    ExceptionHelper.Throw_ArgumentNilError('AAggregator');

  { Retrieve the enumerator object and type }
  Enum := GetEnumerator();

  { Get the first object in the enumeration, otherwise fail! }
  if not Enum.MoveNext() then
    ExceptionHelper.Throw_CollectionEmptyError();

  { Select the first element as comparison base }
  Result := Enum.Current;

  { Iterate over the last N - 1 elements }
  while Enum.MoveNext() do
  begin
    { Aggregate a value }
    Result := AAggregator(Result, Enum.Current);
  end;
end;

function TEnexCollection<T>.AggregateOrDefault(const AAggregator: TFunc<T, T, T>; const ADefault: T): T;
var
  Enum: IEnumerator<T>;
begin
  if not Assigned(AAggregator) then
    ExceptionHelper.Throw_ArgumentNilError('AAggregator');

  { Retrieve the enumerator object and type }
  Enum := GetEnumerator();

  { Get the first object in the enumeration, otherwise fail! }
  if not Enum.MoveNext() then
    Exit(ADefault);

  { Select the first element as comparison base }
  Result := Enum.Current;

  { Iterate over the last N - 1 elements }
  while Enum.MoveNext() do
  begin
    { Aggregate a value }
    Result := AAggregator(Result, Enum.Current);
  end;
end;

function TEnexCollection<T>.All(const APredicate: TFunc<T, Boolean>): Boolean;
var
  Enum: IEnumerator<T>;
begin
  if not Assigned(APredicate) then
    ExceptionHelper.Throw_ArgumentNilError('APredicate');

  { Retrieve the enumerator object }
  Enum := GetEnumerator();

  { Iterate while ALL elements support the predicate }
  while Enum.MoveNext() do
  begin
    if not APredicate(Enum.Current) then
      Exit(false);
  end;

  Result := true;
end;

function TEnexCollection<T>.Any(const APredicate: TFunc<T, Boolean>): Boolean;
var
  Enum: IEnumerator<T>;
begin
  if not Assigned(APredicate) then
    ExceptionHelper.Throw_ArgumentNilError('APredicate');

  { Retrieve the enumerator object }
  Enum := GetEnumerator();

  { Iterate until ANY element supports the predicate }
  while Enum.MoveNext() do
  begin
    if APredicate(Enum.Current) then
      Exit(true);
  end;

  Result := false;
end;

class procedure TEnexCollection<T>.CleanupTypes;
begin
   TType<T>.Unregister;
end;

function TEnexCollection<T>.Concat(const AEnumerable: IEnumerable<T>): IEnexCollection<T>;
begin
  { Check arguments }
  if not Assigned(AEnumerable) then
    ExceptionHelper.Throw_ArgumentNilError('AEnumerable');

  { Create concatenation iterator }
  Result := TEnexConcatCollection<T>.CreateIntf2(Self, AEnumerable, ElementType);
end;

procedure TEnexCollection<T>.Deserialize(const AData: TDeserializationData);
var
  I, LCount: Cardinal;
  LValue: T;
begin
  { Build the serialization info struct }
  StartDeserializing(AData);

  { Start the composite }
  LCount := AData.ExpectListBlock(SSerElements, FElementType.Name);

  if LCount > 0 then
    for I := 0 to LCount - 1 do
    begin
      { Obtain the element }
      FElementType.Deserialize(LValue, AData);

      { Add it to the collection }
      DeserializeElement(LValue);
    end;

  { Stop the process }
  AData.EndBlock();

  EndDeserializing(AData);
end;

procedure TEnexCollection<T>.DeserializeElement(const AElement: T);
begin
 // Do nothing by default.
end;

function TEnexCollection<T>.Distinct: IEnexCollection<T>;
begin
  { Create a new enumerator }
  Result := TEnexDistinctCollection<T>.Create(Self);
end;

function TEnexCollection<T>.ElementAt(const Index: Cardinal): T;
var
  Enum: IEnumerator<T>;
  Count: Cardinal;
begin
  { Retrieve the enumerator object }
  Enum := GetEnumerator();
  Count := 0;

  while Enum.MoveNext() do
  begin
    { If we reached thge element, exit }
    if Count = Index then
      Exit(Enum.Current);

    Inc(Count);
  end;

  { Fail! }
  ExceptionHelper.Throw_ArgumentOutOfRangeError('Index');
end;

function TEnexCollection<T>.ElementAtOrDefault(const Index: Cardinal; const ADefault: T): T;
var
  Enum: IEnumerator<T>;
  Count: Cardinal;
begin
  { Retrieve the enumerator object }
  Enum := GetEnumerator();
  Count := 0;

  while Enum.MoveNext() do
  begin
    { If we reached thge element, exit }
    if Count = Index then
      Exit(Enum.Current);

    Inc(Count);
  end;

  { Return default value }
  Result := ADefault;
end;

function TEnexCollection<T>.EqualsTo(const AEnumerable: IEnumerable<T>): Boolean;
var
  LType: IType<T>;
  LIter1, LIter2: IEnumerator<T>;
  Moved1, Moved2: Boolean;
begin
  { Get the type }
  LType := ElementType;

  { Get enumerators }
  LIter1 := GetEnumerator();
  LIter2 := AEnumerable.GetEnumerator();

  while true do
  begin
    { Iterate and verify that both enumerators moved }
    Moved1 := LIter1.MoveNext();
    Moved2 := LIter2.MoveNext();

    { If one moved but the other did not - error }
    if Moved1 <> Moved2 then
      Exit(false);

    { If neither moved, we've reached the end }
    if not Moved1 then
      break;

    { Verify both values are identical }
    if not LType.AreEqual(LIter1.Current, LIter2.Current) then
      Exit(false);
  end;

  { It worked! }
  Result := true;
end;

function TEnexCollection<T>.Exclude(
  const AEnumerable: IEnumerable<T>): IEnexCollection<T>;
begin
  { Check arguments }
  if not Assigned(AEnumerable) then
    ExceptionHelper.Throw_ArgumentNilError('AEnumerable');

  { Create concatenation iterator }
  Result := TEnexExclusionCollection<T>.CreateIntf2(Self, AEnumerable, ElementType);
end;

class function TEnexCollection<T>.Fill(const AElement: T; const Count: Cardinal; const AType: IType<T>): IEnexCollection<T>;
begin
  { Check arguments }
  if Count = 0 then
    ExceptionHelper.Throw_ArgumentOutOfRangeError('Count');

  if AType = nil then
    ExceptionHelper.Throw_ArgumentNilError('AType');

  { Create an collection }
  Result := TEnexFillCollection<T>.Create(AElement, Count, AType);
end;

class function TEnexCollection<T>.Fill(const AElement: T; const Count: Cardinal): IEnexCollection<T>;
begin
  { Call upper function }
  Result := Fill(AElement, Count, TType<T>.Default);
end;

function TEnexCollection<T>.First: T;
var
  Enum: IEnumerator<T>;
begin
  { Retrieve the enumerator object }
  Enum := GetEnumerator();

  { Get the first object in the enumeration, otherwise fail! }
  if Enum.MoveNext() then
    Result := Enum.Current
  else
    ExceptionHelper.Throw_CollectionEmptyError();
end;

function TEnexCollection<T>.FirstOrDefault(const ADefault: T): T;
var
  Enum: IEnumerator<T>;
begin
  { Retrieve the enumerator object }
  Enum := GetEnumerator();

  { Get the first object in the enumeration, otherwise return default! }
  if Enum.MoveNext() then
    Result := Enum.Current
  else
    Result := ADefault;
end;

function TEnexCollection<T>.Intersect(const AEnumerable: IEnumerable<T>): IEnexCollection<T>;
begin
  { Check arguments }
  if not Assigned(AEnumerable) then
    ExceptionHelper.Throw_ArgumentNilError('AEnumerable');

  { Create concatenation iterator }
  Result := TEnexIntersectionCollection<T>.CreateIntf2(Self, AEnumerable, ElementType);
end;

class function TEnexCollection<T>.Interval(const AStart, AEnd, AIncrement: T): IEnexCollection<T>;
begin
  { Call upper function }
  Result := Interval(AStart, AEnd, AIncrement, TType<T>.Default);
end;

class function TEnexCollection<T>.Interval(const AStart, AEnd, AIncrement: T; const AType: IType<T>): IEnexCollection<T>;
begin
  { Check arguments }
  if not Assigned(AType) then
    ExceptionHelper.Throw_ArgumentNilError('AType');

  { Restrict only to numbers! }
  AType.RestrictTo([tfUnsignedInteger, tfSignedInteger, tfReal]);

  if AType.Compare(AStart, AEnd) >= 0 then
    ExceptionHelper.Throw_ArgumentOutOfRangeError('AStart >= AEnd');

  { Create the collection }
  Result := TEnexIntervalCollection<T>.Create(AStart, AEnd, AIncrement, AType);
end;

class function TEnexCollection<T>.Interval(const AStart, AEnd: T): IEnexCollection<T>;
begin
  { Call upper function }
  Result := Interval(AStart, AEnd, TType<T>.Default);
end;

class function TEnexCollection<T>.Interval(const AStart, AEnd: T; const AType: IType<T>): IEnexCollection<T>;
begin
  { Check arguments }
  if not Assigned(AType) then
    ExceptionHelper.Throw_ArgumentNilError('AType');

  { Restrict only to numbers! }
  AType.RestrictTo([tfUnsignedInteger, tfSignedInteger, tfReal]);

  if AType.Compare(AStart, AEnd) >= 0 then
    ExceptionHelper.Throw_ArgumentOutOfRangeError('AStart >= AEnd');

  { Create the collection }
  Result := TEnexIntervalCollection<T>.Create(AStart, AEnd, AType.ConvertFromVariant(1), AType);
end;

function TEnexCollection<T>.Last: T;
var
  Enum: IEnumerator<T>;
begin
  { Retrieve the enumerator object }
  Enum := GetEnumerator();

  { Get the first object in the enumeration, otherwise fail! }
  if not Enum.MoveNext() then
    ExceptionHelper.Throw_CollectionEmptyError();

  { Iterate till the last element in the enum }
  while true do
  begin
    Result := Enum.Current;

    { Exit if we hit the last element }
    if not Enum.MoveNext() then
      Exit;
  end;
end;

function TEnexCollection<T>.LastOrDefault(const ADefault: T): T;
var
  Enum: IEnumerator<T>;
begin
  { Retrieve the enumerator object }
  Enum := GetEnumerator();

  { Get the first object in the enumeration, otherwise return default! }
  if not Enum.MoveNext() then
    Exit(ADefault);

  { Iterate till the last element in the enum }
  while true do
  begin
    Result := Enum.Current;

    { Exit if we hit the last element }
    if not Enum.MoveNext() then
      Exit;
  end;
end;

function TEnexCollection<T>.Max: T;
var
  Tps: IType<T>;
  Enum: IEnumerator<T>;
begin
  { Retrieve the enumerator object and type }
  Enum := GetEnumerator();
  Tps := ElementType;

  { Get the first object in the enumeration, otherwise fail! }
  if not Enum.MoveNext() then
    ExceptionHelper.Throw_CollectionEmptyError();

  { Select the first element as comparison base }
  Result := Enum.Current;

  { Iterate till the last element in the enum }
  while true do
  begin
    if Tps.Compare(Enum.Current, Result) > 0 then
      Result := Enum.Current;

    { Exit if we hit the last element }
    if not Enum.MoveNext() then
      Exit;
  end;
end;

function TEnexCollection<T>.Min: T;
var
  LType: IType<T>;
  Enum: IEnumerator<T>;
begin
  { Retrieve the enumerator object and type }
  Enum := GetEnumerator();
  LType := ElementType;

  { Get the first object in the enumeration, otherwise fail! }
  if not Enum.MoveNext() then
    ExceptionHelper.Throw_CollectionEmptyError();

  { Select the first element as comparison base }
  Result := Enum.Current;

  { Iterate till the last element in the enum }
  while true do
  begin
    if LType.Compare(Enum.Current, Result) < 0 then
      Result := Enum.Current;

    { Exit if we hit the last element }
    if not Enum.MoveNext() then
      Exit;
  end;
end;

function TEnexCollection<T>.Op: TEnexExtOps<T>;
begin
  { Build up the record + keep an optional reference to the object }
  Result.FInstance := Self;
  Result.FKeepAlive := Self.ExtractReference;
end;

function TEnexCollection<T>.Range(const AStart, AEnd: Cardinal): IEnexCollection<T>;
begin
  { Create a new Enex collection }
  Result := TEnexRangeCollection<T>.Create(Self, AStart, AEnd);
end;

function TEnexCollection<T>.Reversed: IEnexCollection<T>;
var
  List: TList<T>;
begin
  { Create an itermediary list }
  List := TList<T>.Create(Self);
  List.Reverse();

  { Pass the list further }
  Result := List;
end;

procedure TEnexCollection<T>.Serialize(const AData: TSerializationData);
var
  LEnum: IEnumerator<T>;
begin
  { Retrieve the enumerator object and type }
  LEnum := GetEnumerator();

  { Mark the start }
  StartSerializing(AData);

  AData.StartListBlock(SSerElements, FElementType.Name, Count);

  { Serialize all elements in }
  while LEnum.MoveNext() do
    FElementType.Serialize(LEnum.Current, AData);

  { Mark the end }
  AData.EndBlock();

  EndSerializing(AData);
end;

function TEnexCollection<T>.Skip(const ACount: Cardinal): IEnexCollection<T>;
begin
  { Check parameters }
  if ACount = 0 then
    ExceptionHelper.Throw_ArgumentOutOfRangeError('ACount');

  { Create a new Enex collection }
  Result := TEnexSkipCollection<T>.Create(Self, ACount);
end;

function TEnexCollection<T>.SkipWhile(const APredicate: TFunc<T, Boolean>): IEnexCollection<T>;
begin
  { Check arguments }
  if not Assigned(APredicate) then
    ExceptionHelper.Throw_ArgumentNilError('APredicate');

  { Create a new Enex collection }
  Result := TEnexSkipWhileCollection<T>.Create(Self, APredicate);
end;

function TEnexCollection<T>.SkipWhileBetween(const ALower, AHigher: T): IEnexCollection<T>;
var
  LLower, LHigher: T;
  LType: IType<T>;
begin
  { Locals }
  LLower := ALower;
  LHigher := AHigher;
  LType := ElementType;

  { Use SkipWhile() and pass an anonymous function }
  Result := SkipWhile(
    function(Arg1: T): Boolean
    begin
      Exit((LType.Compare(Arg1, LLower) >= 0) and (LType.Compare(Arg1, LHigher) <= 0));
    end
  );
end;

function TEnexCollection<T>.SkipWhileGreater(const ABound: T): IEnexCollection<T>;
var
  LBound: T;
  LType: IType<T>;
begin
  { Locals }
  LBound := ABound;
  LType := ElementType;

  { Use SkipWhile() and pass an anonymous function }
  Result := SkipWhile(
    function(Arg1: T): Boolean
    begin
      Exit(LType.Compare(Arg1, LBound) > 0);
    end
  );
end;

function TEnexCollection<T>.SkipWhileGreaterOrEqual(const ABound: T): IEnexCollection<T>;
var
  LBound: T;
  LType: IType<T>;
begin
  { Locals }
  LBound := ABound;
  LType := ElementType;

  { Use SkipWhile() and pass an anonymous function }
  Result := SkipWhile(
    function(Arg1: T): Boolean
    begin
      Exit(LType.Compare(Arg1, LBound) >= 0);
    end
  );
end;

function TEnexCollection<T>.SkipWhileLower(const ABound: T): IEnexCollection<T>;
var
  LBound: T;
  LType: IType<T>;
begin
  { Locals }
  LBound := ABound;
  LType := ElementType;

  { Use SkipWhile() and pass an anonymous function }
  Result := SkipWhile(
    function(Arg1: T): Boolean
    begin
      Exit(LType.Compare(Arg1, LBound) < 0);
    end
  );
end;

function TEnexCollection<T>.SkipWhileLowerOrEqual(const ABound: T): IEnexCollection<T>;
var
  LBound: T;
  LType: IType<T>;
begin
  { Locals }
  LBound := ABound;
  LType := ElementType;

  { Use SkipWhile() and pass an anonymous function }
  Result := SkipWhile(
    function(Arg1: T): Boolean
    begin
      Exit(LType.Compare(Arg1, LBound) <= 0);
    end
  );
end;

function TEnexCollection<T>.Sorted(const Ascending: Boolean = true): IEnexCollection<T>;
var
  List: TList<T>;
begin
  { Create an itermediary list }
  List := TList<T>.Create(Self);
  List.Sort(Ascending);

  { Pass the list further }
  Result := List;
end;

function TEnexCollection<T>.Take(const ACount: Cardinal): IEnexCollection<T>;
begin
  { Check parameters }
  if ACount = 0 then
    ExceptionHelper.Throw_ArgumentOutOfRangeError('ACount');

  { Create a new Enex collection }
  Result := TEnexTakeCollection<T>.Create(Self, ACount);
end;

function TEnexCollection<T>.TakeWhile(const APredicate: TFunc<T, Boolean>): IEnexCollection<T>;
begin
  { Check arguments }
  if not Assigned(APredicate) then
    ExceptionHelper.Throw_ArgumentNilError('APredicate');

  { Create a new Enex collection }
  Result := TEnexTakeWhileCollection<T>.Create(Self, APredicate);
end;

function TEnexCollection<T>.TakeWhileBetween(const ALower, AHigher: T): IEnexCollection<T>;
var
  LLower, LHigher: T;
  LType: IType<T>;
begin
  { Locals }
  LLower := ALower;
  LHigher := AHigher;
  LType := ElementType;

  { Use TakeWhile() and pass an anonymous function }
  Result := TakeWhile(
    function(Arg1: T): Boolean
    begin
      Exit((LType.Compare(Arg1, LLower) >= 0) and (LType.Compare(Arg1, LHigher) <= 0));
    end
  );
end;

function TEnexCollection<T>.TakeWhileGreater(const ABound: T): IEnexCollection<T>;
var
  LBound: T;
  LType: IType<T>;
begin
  { Locals }
  LBound := ABound;
  LType := ElementType;

  { Use TakeWhile() and pass an anonymous function }
  Result := TakeWhile(
    function(Arg1: T): Boolean
    begin
      Exit(LType.Compare(Arg1, LBound) > 0);
    end
  );
end;

function TEnexCollection<T>.TakeWhileGreaterOrEqual(const ABound: T): IEnexCollection<T>;
var
  LBound: T;
  LType: IType<T>;
begin
  { Locals }
  LBound := ABound;
  LType := ElementType;

  { Use TakeWhile() and pass an anonymous function }
  Result := TakeWhile(
    function(Arg1: T): Boolean
    begin
      Exit(LType.Compare(Arg1, LBound) >= 0);
    end
  );
end;

function TEnexCollection<T>.TakeWhileLower(const ABound: T): IEnexCollection<T>;
var
  LBound: T;
  LType: IType<T>;
begin
  { Locals }
  LBound := ABound;
  LType := ElementType;

  { Use TakeWhile() and pass an anonymous function }
  Result := TakeWhile(
    function(Arg1: T): Boolean
    begin
      Exit(LType.Compare(Arg1, LBound) < 0);
    end
  );
end;

function TEnexCollection<T>.TakeWhileLowerOrEqual(const ABound: T): IEnexCollection<T>;
var
  LBound: T;
  LType: IType<T>;
begin
  { Locals }
  LBound := ABound;
  LType := ElementType;

  { Use TakeWhile() and pass an anonymous function }
  Result := TakeWhile(
    function(Arg1: T): Boolean
    begin
      Exit(LType.Compare(Arg1, LBound) <= 0);
    end
  );
end;

function TEnexCollection<T>.ToList: IList<T>;
begin
  { Simply make up a list }
  Result := TList<T>.Create(Self);
end;

function TEnexCollection<T>.ToSet: ISet<T>;
begin
  { Simply make up a bag }
  Result := THashSet<T>.Create(Self);
end;

function TEnexCollection<T>.Union(
  const AEnumerable: IEnumerable<T>): IEnexCollection<T>;
begin
  { Check arguments }
  if not Assigned(AEnumerable) then
    ExceptionHelper.Throw_ArgumentNilError('AEnumerable');

  { Create concatenation iterator }
  Result := TEnexUnionCollection<T>.CreateIntf2(Self, AEnumerable, ElementType);
end;

function TEnexCollection<T>.Where(const APredicate: TFunc<T, Boolean>): IEnexCollection<T>;
begin
  { Check arguments }
  if not Assigned(APredicate) then
    ExceptionHelper.Throw_ArgumentNilError('APredicate');

  { Create a new Enex collection }
  Result := TEnexWhereCollection<T>.Create(Self, APredicate);
end;

function TEnexCollection<T>.WhereBetween(const ALower, AHigher: T): IEnexCollection<T>;
var
  LLower, LHigher: T;
  LType: IType<T>;
begin
  { Locals }
  LLower := ALower;
  LHigher := AHigher;
  LType := ElementType;

  { Use Where() and pass an anonymous function }
  Result := Where(
    function(Arg1: T): Boolean
    begin
      Exit((LType.Compare(Arg1, LLower) >= 0) and (LType.Compare(Arg1, LHigher) <= 0));
    end
  );
end;

function TEnexCollection<T>.WhereGreater(const ABound: T): IEnexCollection<T>;
var
  LBound: T;
  LType: IType<T>;
begin
  { Locals }
  LBound := ABound;
  LType := ElementType;

  { Use Where() and pass an anonymous function }
  Result := Where(
    function(Arg1: T): Boolean
    begin
      Exit(LType.Compare(Arg1, LBound) > 0);
    end
  );
end;

function TEnexCollection<T>.WhereGreaterOrEqual(const ABound: T): IEnexCollection<T>;
var
  LBound: T;
  LType: IType<T>;
begin
  { Locals }
  LBound := ABound;
  LType := ElementType;

  { Use Where() and pass an anonymous function }
  Result := Where(
    function(Arg1: T): Boolean
    begin
      Exit(LType.Compare(Arg1, LBound) >= 0);
    end
  );
end;

function TEnexCollection<T>.WhereLower(const ABound: T): IEnexCollection<T>;
var
  LBound: T;
  LType: IType<T>;
begin
  { Locals }
  LBound := ABound;
  LType := ElementType;

  { Use Where() and pass an anonymous function }
  Result := Where(
    function(Arg1: T): Boolean
    begin
      Exit(LType.Compare(Arg1, LBound) < 0);
    end
  );
end;

function TEnexCollection<T>.WhereLowerOrEqual(const ABound: T): IEnexCollection<T>;
var
  LBound: T;
  LType: IType<T>;
begin
  { Locals }
  LBound := ABound;
  LType := ElementType;

  { Use Where() and pass an anonymous function }
  Result := Where(
    function(Arg1: T): Boolean
    begin
      Exit(LType.Compare(Arg1, LBound) <= 0);
    end
  );
end;

{ TEnexWhereCollection<T> }

constructor TEnexWhereCollection<T>.Create(const AEnumerable: TEnexCollection<T>; const APredicate: TFunc<T, Boolean>);
begin
  { Check arguments }
  if not Assigned(APredicate) then
    ExceptionHelper.Throw_ArgumentNilError('APredicate');

  if not Assigned(AEnumerable) then
    ExceptionHelper.Throw_ArgumentNilError('AEnumerable');

  { Installing the element type }
  InstallType(Aenumerable.ElementType);

  { Assign internals }
  FEnum := AEnumerable;
  KeepObjectAlive(FEnum);

  FPredicate := APredicate;
  FDeleteEnum := false;
end;

constructor TEnexWhereCollection<T>.CreateIntf(
  const AEnumerable: IEnumerable<T>;
  const APredicate: TFunc<T, Boolean>;
  const AType: IType<T>);
begin
  { Call the upper constructor }
  Create(TEnexWrapCollection<T>.Create(AEnumerable, AType), APredicate);

  { Mark enumerable to be deleted }
  FDeleteEnum := true;
end;

destructor TEnexWhereCollection<T>.Destroy;
begin
  { Delete the enumerable if required }
  ReleaseObject(FEnum, FDeleteEnum);

  inherited;
end;

function TEnexWhereCollection<T>.GetEnumerator: IEnumerator<T>;
begin
  { Generate an enumerator }
  Result := TEnumerator.Create(Self);
end;

{ TEnexWhereCollection<T>.TEnumerator }

constructor TEnexWhereCollection<T>.TEnumerator.Create(const AEnum: TEnexWhereCollection<T>);
begin
  { Initialize }
  FEnum := AEnum;
  KeepObjectAlive(FEnum);

  FIter:= AEnum.FEnum.GetEnumerator();
end;

destructor TEnexWhereCollection<T>.TEnumerator.Destroy;
begin
  ReleaseObject(FEnum);
  inherited;
end;

function TEnexWhereCollection<T>.TEnumerator.GetCurrent: T;
begin
  { Get current element of the "sub-enumerable" object }
  Result := FIter.Current;
end;

function TEnexWhereCollection<T>.TEnumerator.MoveNext: Boolean;
begin
  { Iterate until given condition is met on an element }
  while True do
  begin
    Result := FIter.MoveNext;

    { Terminate on sub-enum termination }
    if not Result then
      Exit;

    { Check whether the current element meets the condition and exit }
    { ... otherwise continue to the next iteration }
    if FEnum.FPredicate(FIter.Current) then
      Exit;
  end;
end;

{ TEnexSelectCollection<T, TOut> }

constructor TEnexSelectCollection<T, TOut>.Create(const AEnumerable: TEnexCollection<T>;
  const ASelector: TFunc<T, TOut>; const AType: IType<TOut>);
begin
  { Check arguments }
  if not Assigned(ASelector) then
    ExceptionHelper.Throw_ArgumentNilError('ASelector');

  if not Assigned(AEnumerable) then
    ExceptionHelper.Throw_ArgumentNilError('AEnumerable');

  if not Assigned(AType) then
    ExceptionHelper.Throw_ArgumentNilError('AType');

  { Installing the element type }
  InstallType(AType);

  { Assign internals }
  FEnum := AEnumerable;
  KeepObjectAlive(FEnum);

  FSelector := ASelector;
  FDeleteEnum := false;
end;

constructor TEnexSelectCollection<T, TOut>.CreateIntf(
  const AEnumerable: IEnumerable<T>;
  const ASelector: TFunc<T, TOut>;
  const AType: IType<TOut>);
begin
  { Call the upper constructor }
  Create(TEnexWrapCollection<T>.Create(AEnumerable, TType<T>.Default), ASelector, AType);

  { Mark enumerable to be deleted }
  FDeleteEnum := true;
end;

destructor TEnexSelectCollection<T, TOut>.Destroy;
begin
  { Delete the enumerable if required }
  ReleaseObject(FEnum, FDeleteEnum);

  inherited;
end;

function TEnexSelectCollection<T, TOut>.ElementAt(const Index: Cardinal): TOut;
begin
  Result := FSelector(FEnum.ElementAt(Index));
end;

function TEnexSelectCollection<T, TOut>.Empty: Boolean;
begin
  Result := FEnum.Empty;
end;

function TEnexSelectCollection<T, TOut>.First: TOut;
begin
  Result := FSelector(FEnum.First);
end;

function TEnexSelectCollection<T, TOut>.GetCount: Cardinal;
begin
  Result := FEnum.GetCount();
end;

function TEnexSelectCollection<T, TOut>.GetEnumerator: IEnumerator<TOut>;
begin
  { Generate an enumerator }
  Result := TEnumerator.Create(Self);
end;

function TEnexSelectCollection<T, TOut>.Last: TOut;
begin
  Result := FSelector(FEnum.Last);
end;

function TEnexSelectCollection<T, TOut>.Single: TOut;
begin
  Result := FSelector(FEnum.Single);
end;

{ TEnexSelectCollection<T, TOut>.TEnumerator }

constructor TEnexSelectCollection<T, TOut>.TEnumerator.Create(const AEnum: TEnexSelectCollection<T, TOut>);
begin
  { Initialize }
  FEnum := AEnum;
  KeepObjectAlive(FEnum);

  FIter := AEnum.FEnum.GetEnumerator();
  FCurrent := default(TOut);
end;

destructor TEnexSelectCollection<T, TOut>.TEnumerator.Destroy;
begin
  ReleaseObject(FEnum);
  inherited;
end;

function TEnexSelectCollection<T, TOut>.TEnumerator.GetCurrent: TOut;
begin
  { Get current element of the "sub-enumerable" object }
  Result := FCurrent;
end;

function TEnexSelectCollection<T, TOut>.TEnumerator.MoveNext: Boolean;
begin
  { Next iteration }
  Result := FIter.MoveNext;

  { Terminate on sub-enum termination }
  if not Result then
    Exit;

  { Return the next "selected" element }
  FCurrent := FEnum.FSelector(FIter.Current);
end;

{ TEnexCastCollection<T, TOut> }

constructor TEnexCastCollection<T, TOut>.Create(const AEnumerable: TEnexCollection<T>; const AOutType: IType<TOut>);
begin
  { Check arguments }
  if not Assigned(AEnumerable) then
    ExceptionHelper.Throw_ArgumentNilError('AEnumerable');

  if not Assigned(AOutType) then
    ExceptionHelper.Throw_ArgumentNilError('AOutType');

  { Installing the element type }
  InstallType(AOutType);

  { Assign internals }
  FEnum := AEnumerable;
  KeepObjectAlive(FEnum);

  FDeleteEnum := false;

  { Create converter }
  FConverter := TConverter<T, TOut>.Create(FEnum.ElementType, AOutType);
end;

constructor TEnexCastCollection<T, TOut>.CreateIntf(
  const AEnumerable: IEnumerable<T>;
  const AInType: IType<T>; const AOutType: IType<TOut>);
begin
  { Call the upper constructor }
  try
    Create(TEnexWrapCollection<T>.Create(AEnumerable, AInType), AOutType);
  finally
    { Mark enumerable to be deleted }
    FDeleteEnum := true;
  end;
end;

destructor TEnexCastCollection<T, TOut>.Destroy;
begin
  { Delete the enumerable if required }
  ReleaseObject(FEnum, FDeleteEnum);

  inherited;
end;

function TEnexCastCollection<T, TOut>.ElementAt(const Index: Cardinal): TOut;
begin
  Result := FConverter.Convert(FEnum.ElementAt(Index));
end;

function TEnexCastCollection<T, TOut>.Empty: Boolean;
begin
  Result := FEnum.Empty;
end;

function TEnexCastCollection<T, TOut>.First: TOut;
begin
  Result := FConverter.Convert(FEnum.First);
end;

function TEnexCastCollection<T, TOut>.GetCount: Cardinal;
begin
  Result := FEnum.GetCount();
end;

function TEnexCastCollection<T, TOut>.GetEnumerator: IEnumerator<TOut>;
begin
  { Generate an enumerator }
  Result := TEnumerator.Create(Self);
end;

function TEnexCastCollection<T, TOut>.Last: TOut;
begin
  Result := FConverter.Convert(FEnum.Last());
end;

function TEnexCastCollection<T, TOut>.Single: TOut;
begin
  Result := FConverter.Convert(FEnum.Single);
end;

{ TEnexCastCollection<T, TOut>.TEnumerator }

constructor TEnexCastCollection<T, TOut>.TEnumerator.Create(const AEnum: TEnexCastCollection<T, TOut>);
begin
  { Initialize }
  FEnum := AEnum;
  KeepObjectAlive(FEnum);

  FIter:= AEnum.FEnum.GetEnumerator();
  FCurrent := default(TOut);
end;

destructor TEnexCastCollection<T, TOut>.TEnumerator.Destroy;
begin
  ReleaseObject(FEnum);
  inherited;
end;

function TEnexCastCollection<T, TOut>.TEnumerator.GetCurrent: TOut;
begin
  { Get current element of the "sub-enumerable" object }
  Result := FCurrent;
end;

function TEnexCastCollection<T, TOut>.TEnumerator.MoveNext: Boolean;
begin
  { Next iteration }
  Result := FIter.MoveNext;

  { Terminate on sub-enum termination }
  if not Result then
    Exit;

  { Return the next "casted" element }
  FCurrent := FEnum.FConverter.Convert(FIter.Current);
end;

{ TEnexConcatCollection<T> }

constructor TEnexConcatCollection<T>.CreateIntf2(
      const AEnumerable1: TEnexCollection<T>;
      const AEnumerable2: IEnumerable<T>; const AType: IType<T>);
begin
  { Call the upper constructor }
  Create(AEnumerable1, TEnexWrapCollection<T>.Create(AEnumerable2, AType));

  { Mark enumerables to be deleted }
  FDeleteEnum2 := true;
end;

constructor TEnexConcatCollection<T>.CreateIntf(
  const AEnumerable1, AEnumerable2: IEnumerable<T>;
  const AType: IType<T>);
begin
  { Call the upper constructor }
  Create(TEnexWrapCollection<T>.Create(AEnumerable1, AType),
         TEnexWrapCollection<T>.Create(AEnumerable2, AType));

  { Mark enumerables to be deleted }
  FDeleteEnum1 := true;
  FDeleteEnum2 := true;
end;

function TEnexConcatCollection<T>.All(const APredicate: TFunc<T, Boolean>): Boolean;
begin
  Result := FEnum1.All(APredicate) and FEnum2.All(APredicate);
end;

function TEnexConcatCollection<T>.Any(const APredicate: TFunc<T, Boolean>): Boolean;
begin
  Result := FEnum1.Any(APredicate) or FEnum2.Any(APredicate);
end;

constructor TEnexConcatCollection<T>.Create(
  const AEnumerable1, AEnumerable2: TEnexCollection<T>);
begin
  { Check arguments }
  if not Assigned(AEnumerable1) then
    ExceptionHelper.Throw_ArgumentNilError('AEnumerable1');

  if not Assigned(AEnumerable2) then
    ExceptionHelper.Throw_ArgumentNilError('AEnumerable2');

  { Installing the element type }
  InstallType(AEnumerable1.ElementType);

  { Assign internals }
  FEnum1 := AEnumerable1;
  KeepObjectAlive(FEnum1);

  FEnum2 := AEnumerable2;
  KeepObjectAlive(FEnum2);

  FDeleteEnum1 := false;
  FDeleteEnum2 := false;
end;

constructor TEnexConcatCollection<T>.CreateIntf1(
  const AEnumerable1: IEnumerable<T>;
  const AEnumerable2: TEnexCollection<T>;
  const AType: IType<T>);
begin
  { Call the upper constructor }
  Create(TEnexWrapCollection<T>.Create(AEnumerable1, AType), AEnumerable2);

  { Mark enumerables to be deleted }
  FDeleteEnum1 := true;
end;

destructor TEnexConcatCollection<T>.Destroy;
begin
  { Delete the enumerable if required }
  ReleaseObject(FEnum1, FDeleteEnum1);
  ReleaseObject(FEnum2, FDeleteEnum2);

  inherited;
end;

function TEnexConcatCollection<T>.Empty: Boolean;
begin
  Result := (GetCount = 0);
end;

function TEnexConcatCollection<T>.GetCount: Cardinal;
begin
  Result := FEnum1.GetCount() + FEnum2.GetCount();
end;

function TEnexConcatCollection<T>.GetEnumerator: IEnumerator<T>;
begin
  { Create enumerator }
  Result := TEnumerator.Create(Self);
end;

{ TEnexConcatCollection<T>.TEnumerator }

constructor TEnexConcatCollection<T>.TEnumerator .Create(const AEnum: TEnexConcatCollection<T>);
begin
  { Initialize }
  FEnum := AEnum;
  KeepObjectAlive(FEnum);

  FIter1 := AEnum.FEnum1.GetEnumerator();
  FIter2 := AEnum.FEnum2.GetEnumerator();
end;

destructor TEnexConcatCollection<T>.TEnumerator .Destroy;
begin
  ReleaseObject(FEnum);
  inherited;
end;

function TEnexConcatCollection<T>.TEnumerator .GetCurrent: T;
begin
  { Pass the first and then the last }
  if FIter1 <> nil then
    Result := FIter1.Current
  else
    Result := FIter2.Current;
end;

function TEnexConcatCollection<T>.TEnumerator .MoveNext: Boolean;
begin
  if FIter1 <> nil then
  begin
    { Iterate over 1 }
    Result := FIter1.MoveNext();

    { Succesefully iterated collection 1 }
    if Result then
      Exit;

    { We've reached the bottom of 1 }
    FIter1 := nil;
  end;

  { Iterate over 2 now }
  Result := FIter2.MoveNext();
end;

{ TEnexUnionCollection<T> }

constructor TEnexUnionCollection<T>.CreateIntf2(
      const AEnumerable1: TEnexCollection<T>;
      const AEnumerable2: IEnumerable<T>; const AType: IType<T>);
begin
  { Call the upper constructor }
  Create(AEnumerable1, TEnexWrapCollection<T>.Create(AEnumerable2, AType));

  { Mark enumerables to be deleted }
  FDeleteEnum2 := true;
end;

constructor TEnexUnionCollection<T>.CreateIntf(
  const AEnumerable1, AEnumerable2: IEnumerable<T>;
  const AType: IType<T>);
begin
  { Call the upper constructor }
  Create(TEnexWrapCollection<T>.Create(AEnumerable1, AType),
         TEnexWrapCollection<T>.Create(AEnumerable2, AType));

  { Mark enumerables to be deleted }
  FDeleteEnum1 := true;
  FDeleteEnum2 := true;
end;

constructor TEnexUnionCollection<T>.Create(
  const AEnumerable1, AEnumerable2: TEnexCollection<T>);
begin
  { Check arguments }
  if not Assigned(AEnumerable1) then
    ExceptionHelper.Throw_ArgumentNilError('AEnumerable1');

  if not Assigned(AEnumerable2) then
    ExceptionHelper.Throw_ArgumentNilError('AEnumerable2');

  { Installing the element type }
  InstallType(AEnumerable1.ElementType);

  { Assign internals }
  FEnum1 := AEnumerable1;
  KeepObjectAlive(FEnum1);

  FEnum2 := AEnumerable2;
  KeepObjectAlive(FEnum2);

  FDeleteEnum1 := false;
  FDeleteEnum2 := false;
end;

constructor TEnexUnionCollection<T>.CreateIntf1(
  const AEnumerable1: IEnumerable<T>;
  const AEnumerable2: TEnexCollection<T>;
  const AType: IType<T>);
begin
  { Call the upper constructor }
  Create(TEnexWrapCollection<T>.Create(AEnumerable1, AType), AEnumerable2);

  { Mark enumerables to be deleted }
  FDeleteEnum1 := true;
end;

destructor TEnexUnionCollection<T>.Destroy;
begin
  { Delete the enumerable if required }
  ReleaseObject(FEnum1, FDeleteEnum1);
  ReleaseObject(FEnum2, FDeleteEnum2);

  inherited;
end;

function TEnexUnionCollection<T>.GetEnumerator: IEnumerator<T>;
begin
  { Create enumerator }
  Result := TEnumerator.Create(Self);
end;

{ TEnexUnionCollection<T>.TEnumerator }

constructor TEnexUnionCollection<T>.TEnumerator .Create(const AEnum: TEnexUnionCollection<T>);
begin
  { Initialize }
  FEnum := AEnum;
  KeepObjectAlive(FEnum);

  FIter1 := AEnum.FEnum1.GetEnumerator();
  FIter2 := AEnum.FEnum2.GetEnumerator();

  { Create an internal set }
  FSet := THashSet<T>.Create(TSuppressedWrapperType<T>.Create(AEnum.FEnum1.ElementType));
end;

destructor TEnexUnionCollection<T>.TEnumerator .Destroy;
begin
  ReleaseObject(FEnum);
  inherited;
end;

function TEnexUnionCollection<T>.TEnumerator .GetCurrent: T;
begin
  { Pass the first and then the last }
  if FIter1 <> nil then
    Result := FIter1.Current
  else
    Result := FIter2.Current;
end;

function TEnexUnionCollection<T>.TEnumerator .MoveNext: Boolean;
begin
  if FIter1 <> nil then
  begin
    { Iterate over 1 }
    Result := FIter1.MoveNext();

    { Succesefully iterated collection 1 }
    if Result then
    begin
      { Add the element to the set }
      FSet.Add(FIter1.Current);
      Exit;
    end;

    { We've reached the bottom of 1 }
    FIter1 := nil;
  end;

  { Continue until we find what we need or we get to the bottom }
  while True do
  begin
    { Iterate over 2 now }
    Result := FIter2.MoveNext();

    { Exit on bad result }
    if not Result then
      Exit;

    { Exit if the element is good }
    if not FSet.Contains(FIter2.Current) then
    begin
      FSet.Add(FIter2.Current);
      Exit;
    end;
  end;
end;

{ TEnexExclusionCollection<T> }

constructor TEnexExclusionCollection<T>.CreateIntf2(
      const AEnumerable1: TEnexCollection<T>;
      const AEnumerable2: IEnumerable<T>; const AType: IType<T>);
begin
  { Call the upper constructor }
  Create(AEnumerable1, TEnexWrapCollection<T>.Create(AEnumerable2, AType));

  { Mark enumerables to be deleted }
  FDeleteEnum2 := true;
end;

constructor TEnexExclusionCollection<T>.CreateIntf(
  const AEnumerable1, AEnumerable2: IEnumerable<T>;
  const AType: IType<T>);
begin
  { Call the upper constructor }
  Create(TEnexWrapCollection<T>.Create(AEnumerable1, AType),
         TEnexWrapCollection<T>.Create(AEnumerable2, AType));

  { Mark enumerables to be deleted }
  FDeleteEnum1 := true;
  FDeleteEnum2 := true;
end;

constructor TEnexExclusionCollection<T>.Create(
  const AEnumerable1, AEnumerable2: TEnexCollection<T>);
begin
  { Check arguments }
  if not Assigned(AEnumerable1) then
    ExceptionHelper.Throw_ArgumentNilError('AEnumerable1');

  if not Assigned(AEnumerable2) then
    ExceptionHelper.Throw_ArgumentNilError('AEnumerable2');

  { Installing the element type }
  InstallType(AEnumerable1.ElementType);

  { Assign internals }
  FEnum1 := AEnumerable1;
  KeepObjectAlive(FEnum1);

  FEnum2 := AEnumerable2;
  KeepObjectAlive(FEnum2);

  FDeleteEnum1 := false;
  FDeleteEnum2 := false;
end;

constructor TEnexExclusionCollection<T>.CreateIntf1(
  const AEnumerable1: IEnumerable<T>;
  const AEnumerable2: TEnexCollection<T>;
  const AType: IType<T>);
begin
  { Call the upper constructor }
  Create(TEnexWrapCollection<T>.Create(AEnumerable1, AType), AEnumerable2);

  { Mark enumerables to be deleted }
  FDeleteEnum1 := true;
end;

destructor TEnexExclusionCollection<T>.Destroy;
begin
  { Delete the enumerable if required }
  ReleaseObject(FEnum1, FDeleteEnum1);
  ReleaseObject(FEnum2, FDeleteEnum2);

  inherited;
end;

function TEnexExclusionCollection<T>.GetEnumerator: IEnumerator<T>;
begin
  { Create enumerator }
  Result := TEnumerator.Create(Self);
end;

{ TEnexExclusionCollection<T>.TEnumerator }

constructor TEnexExclusionCollection<T>.TEnumerator .Create(const AEnum: TEnexExclusionCollection<T>);
begin
  { Initialize }
  FEnum := AEnum;
  KeepObjectAlive(FEnum);

  FIter := AEnum.FEnum1.GetEnumerator();

  { Create an internal set }
  FSet := THashSet<T>.Create(TSuppressedWrapperType<T>.Create(AEnum.FEnum1.ElementType), AEnum.FEnum2);
end;

destructor TEnexExclusionCollection<T>.TEnumerator .Destroy;
begin
  ReleaseObject(FEnum);
  inherited;
end;

function TEnexExclusionCollection<T>.TEnumerator .GetCurrent: T;
begin
  { Pass 1's enumerator }
  Result := FIter.Current;
end;

function TEnexExclusionCollection<T>.TEnumerator .MoveNext: Boolean;
begin
  { Continue until we find what we need or we get to the bottom }
  while True do
  begin
    { Iterate over 1 }
    Result := FIter.MoveNext();

    { Exit on bad result }
    if not Result then
      Exit;

    { Exit if the element is good }
    if not FSet.Contains(FIter.Current) then
      Exit;
  end;
end;


{ TEnexIntersectionCollection<T> }

constructor TEnexIntersectionCollection<T>.CreateIntf2(
      const AEnumerable1: TEnexCollection<T>;
      const AEnumerable2: IEnumerable<T>; const AType: IType<T>);
begin
  { Call the upper constructor }
  Create(AEnumerable1, TEnexWrapCollection<T>.Create(AEnumerable2, AType));

  { Mark enumerables to be deleted }
  FDeleteEnum2 := true;
end;

constructor TEnexIntersectionCollection<T>.CreateIntf(
  const AEnumerable1, AEnumerable2: IEnumerable<T>;
  const AType: IType<T>);
begin
  { Call the upper constructor }
  Create(TEnexWrapCollection<T>.Create(AEnumerable1, AType),
         TEnexWrapCollection<T>.Create(AEnumerable2, AType));

  { Mark enumerables to be deleted }
  FDeleteEnum1 := true;
  FDeleteEnum2 := true;
end;

constructor TEnexIntersectionCollection<T>.Create(
  const AEnumerable1, AEnumerable2: TEnexCollection<T>);
begin
  { Check arguments }
  if not Assigned(AEnumerable1) then
    ExceptionHelper.Throw_ArgumentNilError('AEnumerable1');

  if not Assigned(AEnumerable2) then
    ExceptionHelper.Throw_ArgumentNilError('AEnumerable2');

  { Installing the element type }
  InstallType(AEnumerable1.ElementType);

  { Assign internals }
  FEnum1 := AEnumerable1;
  KeepObjectAlive(FEnum1);

  FEnum2 := AEnumerable2;
  KeepObjectAlive(FEnum2);

  FDeleteEnum1 := false;
  FDeleteEnum2 := false;
end;

constructor TEnexIntersectionCollection<T>.CreateIntf1(
  const AEnumerable1: IEnumerable<T>;
  const AEnumerable2: TEnexCollection<T>;
  const AType: IType<T>);
begin
  { Call the upper constructor }
  Create(TEnexWrapCollection<T>.Create(AEnumerable1, AType), AEnumerable2);

  { Mark enumerables to be deleted }
  FDeleteEnum1 := true;
end;

destructor TEnexIntersectionCollection<T>.Destroy;
begin
  { Delete the enumerable if required }
  ReleaseObject(FEnum1, FDeleteEnum1);
  ReleaseObject(FEnum2, FDeleteEnum2);

  inherited;
end;

function TEnexIntersectionCollection<T>.GetEnumerator: IEnumerator<T>;
begin
  { Create enumerator }
  Result := TEnumerator.Create(Self);
end;

{ TEnexIntersectionCollection<T>.TEnumerator }

constructor TEnexIntersectionCollection<T>.TEnumerator .Create(const AEnum: TEnexIntersectionCollection<T>);
begin
  { Initialize }
  FEnum := AEnum;
  KeepObjectAlive(FEnum);

  FIter := AEnum.FEnum1.GetEnumerator();

  { Create an internal set }
  FSet := THashSet<T>.Create(TSuppressedWrapperType<T>.Create(AEnum.FEnum1.ElementType), AEnum.FEnum2);
end;

destructor TEnexIntersectionCollection<T>.TEnumerator .Destroy;
begin
  ReleaseObject(FEnum);
  inherited;
end;

function TEnexIntersectionCollection<T>.TEnumerator .GetCurrent: T;
begin
  { Pass 1's enumerator }
  Result := FIter.Current;
end;

function TEnexIntersectionCollection<T>.TEnumerator .MoveNext: Boolean;
begin
  { Continue until we find what we need or we get to the bottom }
  while True do
  begin
    { Iterate over 1 }
    Result := FIter.MoveNext();

    { Exit on bad result }
    if not Result then
      Exit;

    { Exit if the element is good }
    if FSet.Contains(FIter.Current) then
      Exit;
  end;
end;

{ TEnexRangeCollection<T> }

constructor TEnexRangeCollection<T>.Create(
  const AEnumerable: TEnexCollection<T>; const AStart,
  AEnd: Cardinal);
begin
  { Check arguments }
  if not Assigned(AEnumerable) then
    ExceptionHelper.Throw_ArgumentNilError('AEnumerable');

  { Installing the element type }
  InstallType(AEnumerable.ElementType);

  { Assign internals }
  FEnum := AEnumerable;
  KeepObjectAlive(FEnum);

  FStart := AStart;
  FEnd := AEnd;
  FDeleteEnum := false;
end;

constructor TEnexRangeCollection<T>.CreateIntf(
  const AEnumerable: IEnumerable<T>; const AStart,
  AEnd: Cardinal; const AType: IType<T>);
begin
  { Call upper constructor }
  Create(TEnexWrapCollection<T>.Create(AEnumerable, AType), AStart, AEnd);

  { Mark for destruction }
  FDeleteEnum := true;
end;

destructor TEnexRangeCollection<T>.Destroy;
begin
  { Delete the enumerable if required }
  ReleaseObject(FEnum, FDeleteEnum);

  inherited;
end;

function TEnexRangeCollection<T>.GetEnumerator: IEnumerator<T>;
begin
  { Create the enumerator }
  Result := TEnumerator.Create(Self);
end;

{ TEnexRangeCollection<T>.TEnumerator }

constructor TEnexRangeCollection<T>.TEnumerator.Create(const AEnum: TEnexRangeCollection<T>);
begin
  { Initialize }
  FEnum := AEnum;
  KeepObjectAlive(FEnum);

  FIter := AEnum.FEnum.GetEnumerator();
  FIdx  := 0;
end;

destructor TEnexRangeCollection<T>.TEnumerator.Destroy;
begin
  ReleaseObject(FEnum);
  inherited;
end;

function TEnexRangeCollection<T>.TEnumerator.GetCurrent: T;
begin
  { PAss the current in the sub-enum }
  Result := FIter.Current;
end;

function TEnexRangeCollection<T>.TEnumerator.MoveNext: Boolean;
begin
  { Skip the required amount of elements }
  if (FIdx <= FEnum.FStart) then
  begin
    while (FIdx <= FEnum.FStart) do
    begin
      { Move cursor }
      Result := FIter.MoveNext();

      if not Result then
        Exit;

      Inc(FIdx);
    end;
  end else
  begin
    { Check if we're finished }
    if (FIdx > FEnum.FEnd) then
      Exit(false);

    { Move the cursor next in the sub-enum, and increase index }
    Result := FIter.MoveNext();
    Inc(FIdx);
  end;
end;

{ TEnexWrapCollection<T> }

constructor TEnexWrapCollection<T>.Create(const AEnumerable: IEnumerable<T>; const AType: IType<T>);
begin
  { Check arguments }
  if not Assigned(AEnumerable) then
    ExceptionHelper.Throw_ArgumentNilError('AEnumerable');

  if not Assigned(AType) then
    ExceptionHelper.Throw_ArgumentNilError('AType');

  { Install the type }
  InstallType(AType);

  { Assign internals }
  FEnum := AEnumerable;
end;

function TEnexWrapCollection<T>.GetEnumerator: IEnumerator<T>;
begin
  { Generate an enumerable from the sub-enum }
  Result := FEnum.GetEnumerator();
end;

{ TEnexDistinctCollection<T> }

constructor TEnexDistinctCollection<T>.Create(const AEnumerable: TEnexCollection<T>);
begin
  { Check arguments }
  if not Assigned(AEnumerable) then
    ExceptionHelper.Throw_ArgumentNilError('AEnumerable');

  { Installing the element type }
  InstallType(AEnumerable.ElementType);

  { Assign internals }
  FEnum := AEnumerable;
  KeepObjectAlive(FEnum);

  FDeleteEnum := false;
end;

constructor TEnexDistinctCollection<T>.CreateIntf(const AEnumerable: IEnumerable<T>; const AType: IType<T>);
begin
  { Call the higher constructor }
  Create(TEnexWrapCollection<T>.Create(AEnumerable, AType));

  { Mark for deletion }
  FDeleteEnum := true;
end;

destructor TEnexDistinctCollection<T>.Destroy;
begin
  { Delete the enumerable if required }
  ReleaseObject(FEnum, FDeleteEnum);

  inherited;
end;

function TEnexDistinctCollection<T>.GetEnumerator: IEnumerator<T>;
begin
  { Create an enumerator }
  Result := TEnumerator.Create(Self);
end;

{ TEnexDistinctCollection<T>.TEnumerator }

constructor TEnexDistinctCollection<T>.TEnumerator.Create(const AEnum: TEnexDistinctCollection<T>);
begin
  { Initialize }
  FEnum := AEnum;
  KeepObjectAlive(FEnum);

  FIter := AEnum.FEnum.GetEnumerator();

  { Create an internal set }
  FSet := THashSet<T>.Create(TSuppressedWrapperType<T>.Create(AEnum.FEnum.ElementType));
end;

destructor TEnexDistinctCollection<T>.TEnumerator.Destroy;
begin
  ReleaseObject(FEnum);
  inherited;
end;

function TEnexDistinctCollection<T>.TEnumerator.GetCurrent: T;
begin
  { Get from sub-enum }
  Result := FIter.Current;
end;

function TEnexDistinctCollection<T>.TEnumerator.MoveNext: Boolean;
begin
  while True do
  begin
    { Iterate }
    Result := FIter.MoveNext;

    if not Result then
      Exit;

    { If the item is distinct, add it to set and continue }
    if not FSet.Contains(FIter.Current) then
    begin
      FSet.Add(FIter.Current);
      Exit;
    end;
  end;
end;

{ TEnexFillCollection<T> }

function TEnexFillCollection<T>.Aggregate(const AAggregator: TFunc<T, T, T>): T;
var
  I: Cardinal;
begin
  { Check arguments }
  if not Assigned(AAggregator) then
    ExceptionHelper.Throw_ArgumentNilError('AAggregator');

  if FCount = 0 then
    ExceptionHelper.Throw_CollectionEmptyError();

  { Select the first element as comparison base }
  Result := FElement;

  { Iterate over the last N - 1 elements }
  for I := 1 to FCount - 1 do
  begin
    { Aggregate a value }
    Result := AAggregator(Result, FElement);
  end;
end;

function TEnexFillCollection<T>.AggregateOrDefault(const AAggregator: TFunc<T, T, T>; const ADefault: T): T;
var
  I: Cardinal;
begin
  { Check arguments }
  if not Assigned(AAggregator) then
    ExceptionHelper.Throw_ArgumentNilError('AAggregator');

  if FCount = 0 then
    Exit(ADefault);

  { Select the first element as comparison base }
  Result := FElement;

  { Iterate over the last N - 1 elements }
  for I := 1 to FCount - 1 do
  begin
    { Aggregate a value }
    Result := AAggregator(Result, FElement);
  end;
end;

function TEnexFillCollection<T>.All(const APredicate: TFunc<T, Boolean>): Boolean;
begin
  if not Assigned(APredicate) then
    ExceptionHelper.Throw_ArgumentNilError('APredicate');

  if not APredicate(FElement) then
    Result := false
  else
    Result := true;
end;

function TEnexFillCollection<T>.Any(const APredicate: TFunc<T, Boolean>): Boolean;
begin
  if not Assigned(APredicate) then
    ExceptionHelper.Throw_ArgumentNilError('APredicate');

  if APredicate(FElement) then
    Result := true
  else
    Result := false;
end;

constructor TEnexFillCollection<T>.Create(const AElement: T; const Count: Cardinal; const AType: IType<T>);
begin
  if Count = 0 then
    ExceptionHelper.Throw_ArgumentOutOfRangeError('Count');

  if AType = nil then
    ExceptionHelper.Throw_ArgumentNilError('AType');

  { Install the type }
  InstallType(AType);

  { Copy values in }
  FCount := Count;
  FElement := AElement;
end;

function TEnexFillCollection<T>.ElementAt(const Index: Cardinal): T;
begin
  if Index = FCount then
    ExceptionHelper.Throw_ArgumentOutOfRangeError('Index');

  Result := FElement;
end;

function TEnexFillCollection<T>.ElementAtOrDefault(const Index: Cardinal; const ADefault: T): T;
begin
  if Index = FCount then
    Result := ADefault
  else
    Result := FElement;
end;

function TEnexFillCollection<T>.Empty: Boolean;
begin
  Result := (FCount = 0);
end;

function TEnexFillCollection<T>.EqualsTo(const AEnumerable: IEnumerable<T>): Boolean;
var
  V: T;
  I: Cardinal;
begin
  I := 0;

  for V in AEnumerable do
  begin
    if I >= FCount then
      Exit(false);

    if not ElementType.AreEqual(FElement, V) then
      Exit(false);

    Inc(I);
  end;

  if I < FCount then
    Exit(false);

  Result := true;
end;

function TEnexFillCollection<T>.First: T;
begin
  if FCount = 0 then
    ExceptionHelper.Throw_CollectionEmptyError();

  Result := FElement;
end;

function TEnexFillCollection<T>.FirstOrDefault(const ADefault: T): T;
begin
  if FCount = 0 then
    Result := ADefault
  else
    Result := FElement;
end;

function TEnexFillCollection<T>.GetCount: Cardinal;
begin
  Result := FCount;
end;

function TEnexFillCollection<T>.GetEnumerator: IEnumerator<T>;
begin
  { Create an enumerator }
  Result := TEnumerator.Create(Self);
end;

function TEnexFillCollection<T>.Last: T;
begin
  if FCount = 0 then
    ExceptionHelper.Throw_CollectionEmptyError();

  Result := FElement;
end;

function TEnexFillCollection<T>.LastOrDefault(const ADefault: T): T;
begin
  if FCount = 0 then
    Result := ADefault
  else
    Result := FElement;
end;

function TEnexFillCollection<T>.Max: T;
begin
  if FCount = 0 then
    ExceptionHelper.Throw_CollectionEmptyError();

  Result := FElement;
end;

function TEnexFillCollection<T>.Min: T;
begin
  if FCount = 0 then
    ExceptionHelper.Throw_CollectionEmptyError();

  Result := FElement;
end;

function TEnexFillCollection<T>.Single: T;
begin
  if FCount = 0 then
    ExceptionHelper.Throw_CollectionEmptyError()
  else if FCount = 1 then
    Result := FElement
  else
    ExceptionHelper.Throw_CollectionHasMoreThanOneElement();
end;

function TEnexFillCollection<T>.SingleOrDefault(const ADefault: T): T;
begin
  if FCount = 0 then
    Result := ADefault
  else if FCount = 1 then
    Result := FElement
  else
    ExceptionHelper.Throw_CollectionHasMoreThanOneElement();
end;


{ TEnexFillCollection<T>.TEnumerator }

constructor TEnexFillCollection<T>.TEnumerator.Create(const AEnum: TEnexFillCollection<T>);
begin
  FEnum := AEnum;
  KeepObjectAlive(FEnum);

  FCount := 0;
end;

destructor TEnexFillCollection<T>.TEnumerator.Destroy;
begin
  ReleaseObject(FEnum);
  inherited;
end;

function TEnexFillCollection<T>.TEnumerator.GetCurrent: T;
begin
  { Pass the element }
  Result := FEnum.FElement;
end;

function TEnexFillCollection<T>.TEnumerator.MoveNext: Boolean;
begin
  { Check for end }
  Result := (FCount < FEnum.FCount);

  if not Result then
    Exit;

  Inc(FCount);
end;

{ TEnexSkipCollection<T> }

constructor TEnexSkipCollection<T>.Create(
  const AEnumerable: TEnexCollection<T>; const ACount: Cardinal);
begin
  { Check parameters }
  if ACount = 0 then
    ExceptionHelper.Throw_ArgumentOutOfRangeError('ACount');

  { Check arguments }
  if not Assigned(AEnumerable) then
    ExceptionHelper.Throw_ArgumentNilError('AEnumerable');

  { Installing the element type }
  InstallType(AEnumerable.ElementType);

  { Assign internals }
  FEnum := AEnumerable;
  KeepObjectAlive(FEnum);

  FCount := ACount;
  FDeleteEnum := false;
end;

constructor TEnexSkipCollection<T>.CreateIntf(
  const AEnumerable: IEnumerable<T>; const ACount: Cardinal; const AType: IType<T>);
begin
  { Call upper constructor }
  Create(TEnexWrapCollection<T>.Create(AEnumerable, AType), ACount);

  { Mark for destruction }
  FDeleteEnum := true;
end;

destructor TEnexSkipCollection<T>.Destroy;
begin
  { Delete the enumerable if required }
  ReleaseObject(FEnum, FDeleteEnum);

  inherited;
end;

function TEnexSkipCollection<T>.GetEnumerator: IEnumerator<T>;
begin
  { Create the enumerator }
  Result := TEnumerator.Create(Self);
end;

{ TEnexSkipCollection<T>.TEnumerator }

constructor TEnexSkipCollection<T>.TEnumerator.Create(const AEnum: TEnexSkipCollection<T>);
begin
  { Initialize }
  FEnum := AEnum;
  KeepObjectAlive(FEnum);

  FIter := AEnum.FEnum.GetEnumerator();
  FIdx  := 0;
end;

destructor TEnexSkipCollection<T>.TEnumerator.Destroy;
begin
  ReleaseObject(FEnum);
  inherited;
end;

function TEnexSkipCollection<T>.TEnumerator.GetCurrent: T;
begin
  { PAss the current in the sub-enum }
  Result := FIter.Current;
end;

function TEnexSkipCollection<T>.TEnumerator.MoveNext: Boolean;
begin
  { Skip the required amount of elements }
  if (FIdx < FEnum.FCount) then
  begin
    while (FIdx < FEnum.FCount) do
    begin
      { Move cursor }
      Result := FIter.MoveNext();

      if not Result then
        Exit;

      Inc(FIdx);
    end;
  end;

  Result := FIter.MoveNext(); { Move the cursor next in the sub-enum }
end;

{ TEnexTakeCollection<T> }

constructor TEnexTakeCollection<T>.Create(
  const AEnumerable: TEnexCollection<T>; const ACount: Cardinal);
begin
  { Check parameters }
  if ACount = 0 then
    ExceptionHelper.Throw_ArgumentOutOfRangeError('ACount');

  { Check arguments }
  if not Assigned(AEnumerable) then
    ExceptionHelper.Throw_ArgumentNilError('AEnumerable');

  { Installing the element type }
  InstallType(AEnumerable.ElementType);

  { Assign internals }
  FEnum := AEnumerable;
  KeepObjectAlive(FEnum);

  FCount := ACount;
  FDeleteEnum := false;
end;

constructor TEnexTakeCollection<T>.CreateIntf(
  const AEnumerable: IEnumerable<T>; const ACount: Cardinal; const AType: IType<T>);
begin
  { Call upper constructor }
  Create(TEnexWrapCollection<T>.Create(AEnumerable, AType), ACount);

  { Mark for destruction }
  FDeleteEnum := true;
end;

destructor TEnexTakeCollection<T>.Destroy;
begin
  { Delete the enumerable if required }
  ReleaseObject(FEnum, FDeleteEnum);

  inherited;
end;

function TEnexTakeCollection<T>.GetEnumerator: IEnumerator<T>;
begin
  { Create the enumerator }
  Result := TEnumerator.Create(Self);
end;

{ TEnexTakeCollection<T>.TEnumerator }

constructor TEnexTakeCollection<T>.TEnumerator.Create(const AEnum: TEnexTakeCollection<T>);
begin
  { Initialize }
  FEnum := AEnum;
  KeepObjectAlive(FEnum);

  FIter := AEnum.FEnum.GetEnumerator();
  FIdx  := 0;
end;

destructor TEnexTakeCollection<T>.TEnumerator.Destroy;
begin
  ReleaseObject(FEnum);
  inherited;
end;

function TEnexTakeCollection<T>.TEnumerator.GetCurrent: T;
begin
  { PAss the current in the sub-enum }
  Result := FIter.Current;
end;

function TEnexTakeCollection<T>.TEnumerator.MoveNext: Boolean;
begin
  { Check if we're finished}
  if (FIdx >= FEnum.FCount) then
    Exit(false);

  { Move the cursor next in the sub-enum, and increase index }
  Result := FIter.MoveNext();
  Inc(FIdx);
end;

{ TEnexTakeWhileCollection<T> }

constructor TEnexTakeWhileCollection<T>.Create(const AEnumerable: TEnexCollection<T>; const APredicate: TFunc<T, Boolean>);
begin
  { Check arguments }
  if not Assigned(APredicate) then
    ExceptionHelper.Throw_ArgumentNilError('APredicate');

  if not Assigned(AEnumerable) then
    ExceptionHelper.Throw_ArgumentNilError('AEnumerable');

  { Install the type }
  InstallType(AEnumerable.ElementType);

  { Assign internals }
  FEnum := AEnumerable;
  KeepObjectAlive(FEnum);

  FPredicate := APredicate;
  FDeleteEnum := false;
end;

constructor TEnexTakeWhileCollection<T>.CreateIntf(
  const AEnumerable: IEnumerable<T>;
  const APredicate: TFunc<T, Boolean>;
  const AType: IType<T>);
begin
  { Call the upper constructor }
  Create(TEnexWrapCollection<T>.Create(AEnumerable, AType), APredicate);

  { Mark enumerable to be deleted }
  FDeleteEnum := true;
end;

destructor TEnexTakeWhileCollection<T>.Destroy;
begin
  { Delete the enumerable if required }
  ReleaseObject(FEnum, FDeleteEnum);

  inherited;
end;

function TEnexTakeWhileCollection<T>.GetEnumerator: IEnumerator<T>;
begin
  { Generate an enumerator }
  Result := TEnumerator.Create(Self);
end;

{ TEnexTakeWhileCollection<T>.TEnumerator }

constructor TEnexTakeWhileCollection<T>.TEnumerator.Create(const AEnum: TEnexTakeWhileCollection<T>);
begin
  { Initialize }
  FEnum := AEnum;
  KeepObjectAlive(FEnum);

  FIter:= AEnum.FEnum.GetEnumerator();
end;

destructor TEnexTakeWhileCollection<T>.TEnumerator.Destroy;
begin
  ReleaseObject(FEnum);
  inherited;
end;

function TEnexTakeWhileCollection<T>.TEnumerator.GetCurrent: T;
begin
  { Get current element of the "sub-enumerable" object }
  Result := FIter.Current;
end;

function TEnexTakeWhileCollection<T>.TEnumerator.MoveNext: Boolean;
begin
  Result := FIter.MoveNext;

  { Terminate on sub-enum termination }
  if not Result then
    Exit;

  { When the condition is not met, stop iterating! }
  if not FEnum.FPredicate(FIter.Current) then
    Exit(false);
end;

{ TEnexSkipWhileCollection<T> }

constructor TEnexSkipWhileCollection<T>.Create(const AEnumerable: TEnexCollection<T>; const APredicate: TFunc<T, Boolean>);
begin
  { Check arguments }
  if not Assigned(APredicate) then
    ExceptionHelper.Throw_ArgumentNilError('APredicate');

  if not Assigned(AEnumerable) then
    ExceptionHelper.Throw_ArgumentNilError('AEnumerable');

  { Install the type }
  InstallType(AEnumerable.ElementType);

  { Assign internals }
  FEnum := AEnumerable;
  KeepObjectAlive(FEnum);

  FPredicate := APredicate;
  FDeleteEnum := false;
end;

constructor TEnexSkipWhileCollection<T>.CreateIntf(
  const AEnumerable: IEnumerable<T>;
  const APredicate: TFunc<T, Boolean>;
  const AType: IType<T>);
begin
  { Call the upper constructor }
  Create(TEnexWrapCollection<T>.Create(AEnumerable, AType), APredicate);

  { Mark enumerable to be deleted }
  FDeleteEnum := true;
end;

destructor TEnexSkipWhileCollection<T>.Destroy;
begin
  { Delete the enumerable if required }
  ReleaseObject(FEnum, FDeleteEnum);

  inherited;
end;

function TEnexSkipWhileCollection<T>.GetEnumerator: IEnumerator<T>;
begin
  { Generate an enumerator }
  Result := TEnumerator.Create(Self);
end;

{ TEnexSkipWhileCollection<T>.TEnumerator }

constructor TEnexSkipWhileCollection<T>.TEnumerator.Create(const AEnum: TEnexSkipWhileCollection<T>);
begin
  { Initialize }
  FEnum := AEnum;
  KeepObjectAlive(FEnum);

  FIter := AEnum.FEnum.GetEnumerator();
  FStop := false;
end;

destructor TEnexSkipWhileCollection<T>.TEnumerator.Destroy;
begin
  ReleaseObject(FEnum);
  inherited;
end;

function TEnexSkipWhileCollection<T>.TEnumerator.GetCurrent: T;
begin
  { Get current element of the "sub-enumerable" object }
  Result := FIter.Current;
end;

function TEnexSkipWhileCollection<T>.TEnumerator.MoveNext: Boolean;
begin
  { Iterate until given condition is met on an element }
  if not FStop then
  begin
    while not FStop do
    begin
      Result := FIter.MoveNext;

      { Terminate on sub-enum termination }
      if not Result then
        Exit;

      { When condition is met, move next }
      if FEnum.FPredicate(FIter.Current) then
        Continue;

      { Mark as skipped }
      FStop := true;
    end;
  end else
    Result := FIter.MoveNext;
end;

{ TEnexIntervalCollection<T> }

constructor TEnexIntervalCollection<T>.Create(const ALower, AHigher, AIncrement: T; const AType: IType<T>);
begin
  { Check arguments }
  if not Assigned(AType) then
    ExceptionHelper.Throw_ArgumentNilError('AType');

  { Restrict only to numbers! }
  AType.RestrictTo([tfUnsignedInteger, tfSignedInteger, tfReal]);

  if AType.Compare(ALower, AHigher) >= 0 then
    ExceptionHelper.Throw_ArgumentOutOfRangeError('ALower >= AHigher');

  { Install the type }
  InstallType(AType);

  { Copy Values }
  FLower := ALower;
  FHigher := AHigher;
  FIncrement := AIncrement;
end;

function TEnexIntervalCollection<T>.Empty: Boolean;
begin
  { Never empty }
  Result := false;
end;

function TEnexIntervalCollection<T>.First: T;
begin
  { Default }
  Result := FLower;
end;

function TEnexIntervalCollection<T>.FirstOrDefault(const ADefault: T): T;
begin
  { Never empty, so - Default }
  Result := FLower;
end;

function TEnexIntervalCollection<T>.GetEnumerator: IEnumerator<T>;
begin
  { Create enumerator }
  Result := TEnumerator.Create(Self);
end;

function TEnexIntervalCollection<T>.Min: T;
begin
  Result := FLower;
end;

{ TEnexIntervalCollection<T>.TEnumerator }

constructor TEnexIntervalCollection<T>.TEnumerator.Create(const AEnum: TEnexIntervalCollection<T>);
begin
  FEnum := AEnum;
  KeepObjectAlive(FEnum);

  FNow := FEnum.FLower;
  FNowVariant := FEnum.ElementType.ConvertToVariant(FNow);
end;

destructor TEnexIntervalCollection<T>.TEnumerator.Destroy;
begin
  ReleaseObject(FEnum);
  inherited;
end;

function TEnexIntervalCollection<T>.TEnumerator.GetCurrent: T;
begin
  { Pass the next value }
  Result := FNow;
end;

function TEnexIntervalCollection<T>.TEnumerator.MoveNext: Boolean;
begin
  FNow := FEnum.ElementType.ConvertFromVariant(FNowVariant);

  { Check bounds }
  Result := (FEnum.ElementType.Compare(FNow, FEnum.FHigher) <= 0);

  if not Result then
    Exit;

  { Update current position }
  FNowVariant := FNowVariant + FEnum.ElementType.ConvertToVariant(FEnum.FIncrement);
end;

{ TCollection<T> }

procedure TCollection<T>.CopyTo(var AArray: array of T);
begin
  { Call upper version }
  CopyTo(AArray, 0);
end;

procedure TCollection<T>.CopyTo(var AArray: array of T; const StartIndex: Cardinal);
var
  Enum: IEnumerator<T>;
  L, I: Cardinal;
begin
  if StartIndex >= Cardinal(Length(AArray)) then
    ExceptionHelper.Throw_ArgumentOutOfRangeError('StartIndex');

  { Retrieve the enumerator object }
  Enum := GetEnumerator();
  L := Cardinal(Length(AArray));
  I := StartIndex;

  { Iterate until ANY element supports the predicate }
  while Enum.MoveNext() do
  begin
    if I >= L then
      ExceptionHelper.Throw_ArgumentOutOfSpaceError('AArray/StartIndex');

    AArray[I] := Enum.Current;
    Inc(I);
  end;
end;

function TCollection<T>.Empty: Boolean;
var
  Enum: IEnumerator<T>;
begin
  { Retrieve the enumerator object }
  Enum := GetEnumerator();

  { Check if empty }
  Result := (not Enum.MoveNext());
end;

procedure TCollection<T>.EndDeserializing(const AData: TDeserializationData);
begin
  // Nothing here, please come again!
end;

procedure TCollection<T>.EndSerializing(const AData: TSerializationData);
begin
  // Nothing here, please take your bussiness elsewhere!
end;

function TCollection<T>.GetCount: Cardinal;
var
  Enum: IEnumerator<T>;
begin
  { Retrieve the enumerator object }
  Enum := GetEnumerator();

  { Iterate till the end }
  Result := 0;
  while Enum.MoveNext() do Inc(Result);
end;

function TCollection<T>.Single: T;
var
  Enum: IEnumerator<T>;
begin
  { Retrieve the enumerator object }
  Enum := GetEnumerator();

  { Get the first object in the enumeration, otherwise fail! }
  if Enum.MoveNext() then
    Result := Enum.Current
  else
    ExceptionHelper.Throw_CollectionEmptyError();

  { Fail if more than one elements are there }
  if Enum.MoveNext() then
    ExceptionHelper.Throw_CollectionHasMoreThanOneElement();
end;

function TCollection<T>.SingleOrDefault(const ADefault: T): T;
var
  Enum: IEnumerator<T>;
begin
  { Retrieve the enumerator object }
  Enum := GetEnumerator();

  { Get the first object in the enumeration, otherwise fail! }
  if Enum.MoveNext() then
    Result := Enum.Current
  else
    Exit(ADefault);

  { Fail if more than one elements are there }
  if Enum.MoveNext() then
    ExceptionHelper.Throw_CollectionHasMoreThanOneElement();
end;

procedure TCollection<T>.StartDeserializing(const AData: TDeserializationData);
begin
  { Unsupported by default }
  ExceptionHelper.Throw_Unserializable(AData.CurrentElementInfo.Name, ClassName);
end;

procedure TCollection<T>.StartSerializing(const AData: TSerializationData);
begin
  { Unsupported by default }
  ExceptionHelper.Throw_Unserializable(AData.CurrentElementInfo.Name, ClassName);
end;

function TCollection<T>.ToArray: TArray<T>;
var
  LCount: Cardinal;
  LResult: TArray<T>;
begin
  LCount := Count;

  if LCount > 0 then
  begin
    { Set the length of array }
    SetLength(LResult, LCount);

    { Copy all elements to array }
    CopyTo(LResult);
  end else
    SetLength(LResult, 0);

  Result := LResult;
end;

function TCollection<T>.ToDynamicArray: TDynamicArray<T>;
var
  LCount: Cardinal;
  LArray: TArray<T>;

begin
  LCount := Count;

  if LCount > 0 then
  begin
    { Set the length of array }
    SetLength(LArray, LCount);

    { Copy all elements to array }
    CopyTo(LArray);
  end;

  Result := TDynamicArray<T>.Consume(LArray);
end;

function TCollection<T>.ToFixedArray: TFixedArray<T>;
var
  LCount: Cardinal;
  LArray: TArray<T>;

begin
  LCount := Count;

  if LCount > 0 then
  begin
    { Set the length of array }
    SetLength(LArray, LCount);

    { Copy all elements to array }
    CopyTo(LArray);
  end;

  Result := TFixedArray<T>.Consume(LArray);
end;

{ TEnexAssociativeCollection<TKey, TValue> }

class procedure TEnexAssociativeCollection<TKey, TValue>.CleanupTypes;
begin
   TType<TKey>.Unregister;
   TType<TValue>.Unregister;
end;

procedure TEnexAssociativeCollection<TKey, TValue>.Deserialize(const AData: TDeserializationData);
var
  I, LCount: Cardinal;
  LKey: TKey;
  LValue: TValue;
begin
  StartDeserializing(AData);

  { Open up the composite }
  LCount := AData.ExpectListBlock(SSerElements, SSerPair);

  if LCount > 0 then
    for I := 0 to LCount - 1 do
    begin
      { Open the scope for the K/V pair }
      AData.ExpectBlock();

      { Obtain the element }
      FKeyType.Deserialize(SSerKey, LKey, AData);
      FValueType.Deserialize(SSerValue, LValue, AData);

      AData.EndBlock();

      { Add it to the collection }
      DeserializePair(LKey, LValue);
    end;

  { Stop the process }
  AData.EndBlock();
  EndDeserializing(AData);
end;

procedure TEnexAssociativeCollection<TKey, TValue>.DeserializePair(const AKey: TKey; const AValue: TValue);
begin
  // Nothing here ...
end;

function TEnexAssociativeCollection<TKey, TValue>.DistinctByKeys: IEnexAssociativeCollection<TKey, TValue>;
begin
  Result := TEnexAssociativeDistinctByKeysCollection<TKey, TValue>.Create(Self);
end;

function TEnexAssociativeCollection<TKey, TValue>.DistinctByValues: IEnexAssociativeCollection<TKey, TValue>;
begin
  Result := TEnexAssociativeDistinctByValuesCollection<TKey, TValue>.Create(Self);
end;

function TEnexAssociativeCollection<TKey, TValue>.Includes(const AEnumerable: IEnumerable<TKeyValuePair<TKey, TValue>>): Boolean;
var
  Enum: IEnumerator<TKeyValuePair<TKey, TValue>>;
begin
  { Retrieve the enumerator object }
  Enum := AEnumerable.GetEnumerator();

  { Iterate till the last element in the enum }
  while Enum.MoveNext do
  begin
    if not KeyHasValue(Enum.Current.Key, Enum.Current.Value) then
      Exit(false);
  end;

  { We got here, it means all is OK }
  Result := true;
end;

procedure TEnexAssociativeCollection<TKey, TValue>.InstallTypes(const AKeyType: IType<TKey>; const AValueType: IType<TValue>);
begin
  FKeyType := AKeyType;
  FValueType := AValueType;
end;

function TEnexAssociativeCollection<TKey, TValue>.KeyHasValue(const AKey: TKey; const AValue: TValue): Boolean;
var
  Enum: IEnumerator<TKeyValuePair<TKey, TValue>>;
begin
  { Retrieve the enumerator object and type }
  Enum := GetEnumerator();

  { Iterate till the last element in the enum }
  while Enum.MoveNext do
  begin
    if KeyType.AreEqual(Enum.Current.Key, AKey) and
       ValueType.AreEqual(Enum.Current.Value, AValue) then
      Exit(true);
  end;

  { No found! }
  Result := false;
end;

function TEnexAssociativeCollection<TKey, TValue>.MaxKey: TKey;
var
  Enum: IEnumerator<TKeyValuePair<TKey, TValue>>;
begin
  { Retrieve the enumerator object and type }
  Enum := GetEnumerator();

  { Get the first object in the enumeration, otherwise fail! }
  if not Enum.MoveNext() then
    ExceptionHelper.Throw_CollectionEmptyError();

  { Select the first element as comparison base }
  Result := Enum.Current.Key;

  { Iterate till the last element in the enum }
  while true do
  begin
    if KeyType.Compare(Enum.Current.Key, Result) > 0 then
      Result := Enum.Current.Key;

    { Exit if we hit the last element }
    if not Enum.MoveNext() then
      Exit;
  end;
end;

function TEnexAssociativeCollection<TKey, TValue>.MaxValue: TValue;
var
  Enum: IEnumerator<TKeyValuePair<TKey, TValue>>;
begin
  { Retrieve the enumerator object and type }
  Enum := GetEnumerator();

  { Get the first object in the enumeration, otherwise fail! }
  if not Enum.MoveNext() then
    ExceptionHelper.Throw_CollectionEmptyError();

  { Select the first element as comparison base }
  Result := Enum.Current.Value;

  { Iterate till the last element in the enum }
  while true do
  begin
    if ValueType.Compare(Enum.Current.Value, Result) > 0 then
      Result := Enum.Current.Value;

    { Exit if we hit the last element }
    if not Enum.MoveNext() then
      Exit;
  end;
end;

function TEnexAssociativeCollection<TKey, TValue>.MinKey: TKey;
var
  Enum: IEnumerator<TKeyValuePair<TKey, TValue>>;
begin
  { Retrieve the enumerator object and type }
  Enum := GetEnumerator();

  { Get the first object in the enumeration, otherwise fail! }
  if not Enum.MoveNext() then
    ExceptionHelper.Throw_CollectionEmptyError();

  { Select the first element as comparison base }
  Result := Enum.Current.Key;

  { Iterate till the last element in the enum }
  while true do
  begin
    if KeyType.Compare(Enum.Current.Key, Result) < 0 then
      Result := Enum.Current.Key;

    { Exit if we hit the last element }
    if not Enum.MoveNext() then
      Exit;
  end;
end;

function TEnexAssociativeCollection<TKey, TValue>.MinValue: TValue;
var
  Enum: IEnumerator<TKeyValuePair<TKey, TValue>>;
begin
  { Retrieve the enumerator object and type }
  Enum := GetEnumerator();

  { Get the first object in the enumeration, otherwise fail! }
  if not Enum.MoveNext() then
    ExceptionHelper.Throw_CollectionEmptyError();

  { Select the first element as comparison base }
  Result := Enum.Current.Value;

  { Iterate till the last element in the enum }
  while true do
  begin
    if ValueType.Compare(Enum.Current.Value, Result) < 0 then
      Result := Enum.Current.Value;

    { Exit if we hit the last element }
    if not Enum.MoveNext() then
      Exit;
  end;
end;

function TEnexAssociativeCollection<TKey, TValue>.SelectKeys: IEnexCollection<TKey>;
begin
  { Create a selector }
  Result := TEnexSelectKeysCollection<TKey, TValue>.Create(Self);
end;

function TEnexAssociativeCollection<TKey, TValue>.SelectValues: IEnexCollection<TValue>;
begin
  { Create a selector }
  Result := TEnexSelectValuesCollection<TKey, TValue>.Create(Self);
end;

procedure TEnexAssociativeCollection<TKey, TValue>.Serialize(const AData: TSerializationData);
var
  LEnum: IEnumerator<TKeyValuePair<TKey, TValue>>;
  LKeyInfo, LValInfo, LElemInfo: TValueInfo;
begin
  { Retrieve the enumerator object and type }
  LEnum := GetEnumerator();

  LKeyInfo := TValueInfo.Create(SSerKey);
  LValInfo := TValueInfo.Create(SSerValue);
  LElemInfo := TValueInfo.Indexed;

  { Mark the start }
  StartSerializing(AData);

  AData.StartListBlock(SSerElements, SSerPair, Count);

  { Serialize all elements in }
  while LEnum.MoveNext() do
  begin
    { Open the scope for the K/V pair }
    AData.StartBlock();

    { Serialize the K/V pair }
    FKeyType.Serialize(SSerKey, LEnum.Current.Key, AData);
    FValueType.Serialize(SSerValue, LEnum.Current.Value, AData);

    AData.EndBlock();
  end;

  { The end }
  AData.EndBlock();

  EndSerializing(AData);
end;

function TEnexAssociativeCollection<TKey, TValue>.ToDictionary: IDictionary<TKey, TValue>;
begin
  Result := TDictionary<TKey, TValue>.Create(Self);
end;

function TEnexAssociativeCollection<TKey, TValue>.ValueForKey(const AKey: TKey): TValue;
var
  Enum: IEnumerator<TKeyValuePair<TKey, TValue>>;
begin
  { Retrieve the enumerator object and type }
  Enum := GetEnumerator();

  { Iterate till the last element in the enum }
  while Enum.MoveNext do
  begin
    if KeyType.AreEqual(Enum.Current.Key, AKey) then
      Exit(Enum.Current.Value);
  end;

  { If nothing found, simply raise an exception }
  ExceptionHelper.Throw_KeyNotFoundError(KeyType.GetString(AKey));
end;

function TEnexAssociativeCollection<TKey, TValue>.Where(
  const APredicate: TFunc<TKey, TValue, Boolean>): IEnexAssociativeCollection<TKey, TValue>;
begin
  { Check arguments }
  if not Assigned(APredicate) then
    ExceptionHelper.Throw_ArgumentNilError('APredicate');

  { Create a new Enex collection }
  Result := TEnexAssociativeWhereCollection<TKey, TValue>.Create(Self, APredicate);
end;

function TEnexAssociativeCollection<TKey, TValue>.WhereKeyBetween(const ALower,
  AHigher: TKey): IEnexAssociativeCollection<TKey, TValue>;
var
  LLower, LHigher: TKey;
  LType: IType<TKey>;
begin
  { Locals }
  LLower := ALower;
  LHigher := AHigher;

  LType := KeyType;

  { Use Where() and pass an anonymous function }
  Result := Where(
    function(Arg1: TKey; Arg2: TValue): Boolean
    begin
      Exit((LType.Compare(Arg1, LLower) >= 0) and (LType.Compare(Arg1, LHigher) <= 0));
    end
  );
end;

function TEnexAssociativeCollection<TKey, TValue>.WhereKeyGreater(
  const ABound: TKey): IEnexAssociativeCollection<TKey, TValue>;
var
  LBound: TKey;
  LType: IType<TKey>;
begin
  { Locals }
  LBound := ABound;

  LType := KeyType;

  { Use Where() and pass an anonymous function }
  Result := Where(
    function(Arg1: TKey; Arg2: TValue): Boolean
    begin
      Exit(LType.Compare(Arg1, LBound) > 0);
    end
  );
end;

function TEnexAssociativeCollection<TKey, TValue>.WhereKeyGreaterOrEqual(
  const ABound: TKey): IEnexAssociativeCollection<TKey, TValue>;
var
  LBound: TKey;
  LType: IType<TKey>;
begin
  { Locals }
  LBound := ABound;

  LType := KeyType;

  { Use Where() and pass an anonymous function }
  Result := Where(
    function(Arg1: TKey; Arg2: TValue): Boolean
    begin
      Exit(LType.Compare(Arg1, LBound) >= 0);
    end
  );
end;

function TEnexAssociativeCollection<TKey, TValue>.WhereKeyLower(
  const ABound: TKey): IEnexAssociativeCollection<TKey, TValue>;
var
  LBound: TKey;
  LType: IType<TKey>;
begin
  { Locals }
  LBound := ABound;

  LType := KeyType;

  { Use Where() and pass an anonymous function }
  Result := Where(
    function(Arg1: TKey; Arg2: TValue): Boolean
    begin
      Exit(LType.Compare(Arg1, LBound) < 0);
    end
  );
end;

function TEnexAssociativeCollection<TKey, TValue>.WhereKeyLowerOrEqual(
  const ABound: TKey): IEnexAssociativeCollection<TKey, TValue>;
var
  LBound: TKey;
  LType: IType<TKey>;
begin
  { Locals }
  LBound := ABound;

  LType := KeyType;

  { Use Where() and pass an anonymous function }
  Result := Where(
    function(Arg1: TKey; Arg2: TValue): Boolean
    begin
      Exit(LType.Compare(Arg1, LBound) <= 0);
    end
  );
end;

function TEnexAssociativeCollection<TKey, TValue>.WhereValueBetween(
  const ALower, AHigher: TValue): IEnexAssociativeCollection<TKey, TValue>;
var
  LLower, LHigher: TValue;
  LType: IType<TValue>;
begin
  { Locals }
  LLower := ALower;
  LHigher := AHigher;

  LType := ValueType;

  { Use Where() and pass an anonymous function }
  Result := Where(
    function(Arg1: TKey; Arg2: TValue): Boolean
    begin
      Exit((LType.Compare(Arg2, LLower) >= 0) and (LType.Compare(Arg2, LHigher) <= 0));
    end
  );
end;

function TEnexAssociativeCollection<TKey, TValue>.WhereValueGreater(
  const ABound: TValue): IEnexAssociativeCollection<TKey, TValue>;
var
  LBound: TValue;
  LType: IType<TValue>;
begin
  { Locals }
  LBound := ABound;

  LType := ValueType;

  { Use Where() and pass an anonymous function }
  Result := Where(
    function(Arg1: TKey; Arg2: TValue): Boolean
    begin
      Exit(LType.Compare(Arg2, LBound) > 0);
    end
  );
end;

function TEnexAssociativeCollection<TKey, TValue>.WhereValueGreaterOrEqual(
  const ABound: TValue): IEnexAssociativeCollection<TKey, TValue>;
var
  LBound: TValue;
  LType: IType<TValue>;
begin
  { Locals }
  LBound := ABound;

  LType := ValueType;

  { Use Where() and pass an anonymous function }
  Result := Where(
    function(Arg1: TKey; Arg2: TValue): Boolean
    begin
      Exit(LType.Compare(Arg2, LBound) >= 0);
    end
  );
end;

function TEnexAssociativeCollection<TKey, TValue>.WhereValueLower(
  const ABound: TValue): IEnexAssociativeCollection<TKey, TValue>;
var
  LBound: TValue;
  LType: IType<TValue>;
begin
  { Locals }
  LBound := ABound;

  LType := ValueType;

  { Use Where() and pass an anonymous function }
  Result := Where(
    function(Arg1: TKey; Arg2: TValue): Boolean
    begin
      Exit(LType.Compare(Arg2, LBound) < 0);
    end
  );
end;

function TEnexAssociativeCollection<TKey, TValue>.WhereValueLowerOrEqual(
  const ABound: TValue): IEnexAssociativeCollection<TKey, TValue>;
var
  LBound: TValue;
  LType: IType<TValue>;
begin
  { Locals }
  LBound := ABound;

  LType := ValueType;

  { Use Where() and pass an anonymous function }
  Result := Where(
    function(Arg1: TKey; Arg2: TValue): Boolean
    begin
      Exit(LType.Compare(Arg2, LBound) <= 0);
    end
  );
end;

{ TEnexSelectKeysCollection<TKey, TValue> }

constructor TEnexSelectKeysCollection<TKey, TValue>.Create(const AEnumerable: TEnexAssociativeCollection<TKey, TValue>);
begin
  { Check arguments }
  if not Assigned(AEnumerable) then
    ExceptionHelper.Throw_ArgumentNilError('AEnumerable');

  { Install the type }
  InstallType(AEnumerable.KeyType);

  { Assign internals }
  FEnum := AEnumerable;

  KeepObjectAlive(FEnum);
  FDeleteEnum := false;
end;

constructor TEnexSelectKeysCollection<TKey, TValue>.CreateIntf(
  const AEnumerable: IEnumerable<TKEyValuePair<TKey, TValue>>;
  const AKeyType: IType<TKey>; const AValueType: IType<TValue>);
begin
  { Call the upper constructor }
  Create(TEnexAssociativeWrapCollection<TKey, TValue>.Create(AEnumerable, AKeyType, AValueType));

  { Mark enumerable to be deleted }
  FDeleteEnum := true;
end;

destructor TEnexSelectKeysCollection<TKey, TValue>.Destroy;
begin
  { Delete the enumerable if required }
  ReleaseObject(FEnum, FDeleteEnum);

  inherited;
end;

function TEnexSelectKeysCollection<TKey, TValue>.GetCount: Cardinal;
begin
  Result := FEnum.GetCount();
end;

function TEnexSelectKeysCollection<TKey, TValue>.GetEnumerator: IEnumerator<TKey>;
begin
  { Generate an enumerator }
  Result := TEnumerator.Create(Self);
end;

{ TEnexSelectKeysCollection<TKey, TValue>.TEnumerator }

constructor TEnexSelectKeysCollection<TKey, TValue>.TEnumerator.Create(
  const AEnum: TEnexSelectKeysCollection<TKey, TValue>);
begin
  { Initialize }
  FEnum := AEnum;
  KeepObjectAlive(FEnum);

  FIter:= AEnum.FEnum.GetEnumerator();
  FCurrent := default(TKey);
end;

destructor TEnexSelectKeysCollection<TKey, TValue>.TEnumerator.Destroy;
begin
  ReleaseObject(FEnum);
  inherited;
end;

function TEnexSelectKeysCollection<TKey, TValue>.TEnumerator.GetCurrent: TKey;
begin
  { Get current element of the "sub-enumerable" object }
  Result := FCurrent;
end;

function TEnexSelectKeysCollection<TKey, TValue>.TEnumerator.MoveNext: Boolean;
begin
  { Next iteration }
  Result := FIter.MoveNext;

  { Terminate on sub-enum termination }
  if not Result then
    Exit;

  { Return the next "selected" key }
  FCurrent := FIter.Current.Key;
end;

{ TEnexSelectValuesCollection<TKey, TValue> }

constructor TEnexSelectValuesCollection<TKey, TValue>.Create(
  const AEnumerable: TEnexAssociativeCollection<TKey, TValue>);
begin
  { Check arguments }
  if not Assigned(AEnumerable) then
    ExceptionHelper.Throw_ArgumentNilError('AEnumerable');

  { Install the type }
  InstallType(AEnumerable.ValueType);

  { Assign internals }
  FEnum := AEnumerable;

  KeepObjectAlive(FEnum);
  FDeleteEnum := false;
end;

constructor TEnexSelectValuesCollection<TKey, TValue>.CreateIntf(
  const AEnumerable: IEnumerable<TKeyValuePair<TKey, TValue>>;
  const AKeyType: IType<TKey>;
  const AValueType: IType<TValue>);
begin
  { Call the upper constructor }
  Create(TEnexAssociativeWrapCollection<TKey, TValue>.Create(AEnumerable, AKeyType, AValueType));

  { Mark enumerable to be deleted }
  FDeleteEnum := true;
end;

destructor TEnexSelectValuesCollection<TKey, TValue>.Destroy;
begin
  { Delete the enumerable if required }
  ReleaseObject(FEnum, FDeleteEnum);

  inherited;
end;

function TEnexSelectValuesCollection<TKey, TValue>.GetCount: Cardinal;
begin
  Result := FEnum.GetCount();
end;

function TEnexSelectValuesCollection<TKey, TValue>.GetEnumerator: IEnumerator<TValue>;
begin
  { Generate an enumerator }
  Result := TEnumerator.Create(Self);
end;

{ TEnexSelectValuesCollection<TKey, TValue>.TEnumerator }

constructor TEnexSelectValuesCollection<TKey, TValue>.TEnumerator.Create(
  const AEnum: TEnexSelectValuesCollection<TKey, TValue>);
begin
  { Initialize }
  FEnum := AEnum;
  KeepObjectAlive(FEnum);

  FIter:= AEnum.FEnum.GetEnumerator();
  FCurrent := default(TValue);
end;

destructor TEnexSelectValuesCollection<TKey, TValue>.TEnumerator.Destroy;
begin
  ReleaseObject(FEnum);
  inherited;
end;

function TEnexSelectValuesCollection<TKey, TValue>.TEnumerator.GetCurrent: TValue;
begin
  { Get current element of the "sub-enumerable" object }
  Result := FCurrent;
end;

function TEnexSelectValuesCollection<TKey, TValue>.TEnumerator.MoveNext: Boolean;
begin
  { Next iteration }
  Result := FIter.MoveNext;

  { Terminate on sub-enum termination }
  if not Result then
    Exit;

  { Return the next "selected" key }
  FCurrent := FIter.Current.Value;
end;

{ TEnexAssociativeWhereCollection<TKey, TValue> }

constructor TEnexAssociativeWhereCollection<TKey, TValue>.Create(
  const AEnumerable: TEnexAssociativeCollection<TKey, TValue>;
  const APredicate: TFunc<TKey, TValue, Boolean>);
begin
  { Check arguments }
  if not Assigned(APredicate) then
    ExceptionHelper.Throw_ArgumentNilError('APredicate');

  if not Assigned(AEnumerable) then
    ExceptionHelper.Throw_ArgumentNilError('AEnumerable');

  { Install types }
  InstallTypes(AEnumerable.KeyType, AEnumerable.ValueType);

  { Assign internals }
  FEnum := AEnumerable;
  KeepObjectAlive(FEnum);

  FPredicate := APredicate;
  FDeleteEnum := false;
end;

constructor TEnexAssociativeWhereCollection<TKey, TValue>.CreateIntf(
  const AEnumerable: IEnumerable<TKeyValuePair<TKey, TValue>>;
  const APredicate: TFunc<TKey, TValue, Boolean>;
  const AKeyType: IType<TKey>;
  const AValueType: IType<TValue>);
begin
  { Call the upper constructor }
  Create(TEnexAssociativeWrapCollection<TKey, TValue>.Create(AEnumerable, AKeyType, AValueType), APredicate);

  { Mark enumerable to be deleted }
  FDeleteEnum := true;
end;

destructor TEnexAssociativeWhereCollection<TKey, TValue>.Destroy;
begin
  { Delete the enumerable if required }
  ReleaseObject(FEnum, FDeleteEnum);

  inherited;
end;

function TEnexAssociativeWhereCollection<TKey, TValue>.GetEnumerator: IEnumerator<TKeyValuePair<TKey, TValue>>;
begin
  { Generate an enumerator }
  Result := TEnumerator.Create(Self);
end;

{ TEnexAssociativeWhereCollection<TKey, TValue>.TEnumerator }

constructor TEnexAssociativeWhereCollection<TKey, TValue>.TEnumerator.Create(
  const AEnum: TEnexAssociativeWhereCollection<TKey, TValue>);
begin
  { Initialize }
  FEnum := AEnum;
  KeepObjectAlive(FEnum);

  FIter := AEnum.FEnum.GetEnumerator();
end;

destructor TEnexAssociativeWhereCollection<TKey, TValue>.TEnumerator.Destroy;
begin
  ReleaseObject(FEnum);
  inherited;
end;

function TEnexAssociativeWhereCollection<TKey, TValue>.TEnumerator.GetCurrent: TKeyValuePair<TKey, TValue>;
begin
  { Get current element of the "sub-enumerable" object }
  Result := FIter.Current;
end;

function TEnexAssociativeWhereCollection<TKey, TValue>.TEnumerator.MoveNext: Boolean;
begin
  { Iterate until given condition is met on an element }
  while True do
  begin
    Result := FIter.MoveNext;

    { Terminate on sub-enum termination }
    if not Result then
      Exit;

    { Check whether the current element meets the condition and exit }
    { ... otherwise continue to the next iteration }
    if FEnum.FPredicate(FIter.Current.Key, FIter.Current.Value) then
      Exit;
  end;
end;

{ TEnexAssociativeWrapCollection<TKey, TValue> }

constructor TEnexAssociativeWrapCollection<TKey, TValue>.Create(
  const AEnumerable: IEnumerable<TKeyValuePair<TKey, TValue>>;
  const AKeyType: IType<TKey>;
  const AValueType: IType<TValue>);
begin
  { Check arguments }
  if not Assigned(AEnumerable) then
    ExceptionHelper.Throw_ArgumentNilError('AEnumerable');

  if not Assigned(AKeyType) then
    ExceptionHelper.Throw_ArgumentNilError('AKeyType');

  if not Assigned(AValueType) then
    ExceptionHelper.Throw_ArgumentNilError('AValueType');

  { Install both types }
  InstallTypes(AKeyType, AValueType);

  { Assign internals }
  FEnum := AEnumerable;
end;

function TEnexAssociativeWrapCollection<TKey, TValue>.GetEnumerator: IEnumerator<TKeyValuePair<TKey, TValue>>;
begin
  { Generate an enumerable from the sub-enum }
  Result := FEnum.GetEnumerator();
end;

{ TEnexAssociativeDistinctByKeysCollection<TKey, TValue> }

constructor TEnexAssociativeDistinctByKeysCollection<TKey, TValue>.Create(
  const AEnumerable: TEnexAssociativeCollection<TKey, TValue>);
begin
  { Check arguments }
  if not Assigned(AEnumerable) then
    ExceptionHelper.Throw_ArgumentNilError('AEnumerable');

  { Install types }
  InstallTypes(AEnumerable.KeyType, AEnumerable.ValueType);

  { Assign internals }
  FEnum := AEnumerable;
  KeepObjectAlive(FEnum);

  FDeleteEnum := false;
end;

constructor TEnexAssociativeDistinctByKeysCollection<TKey, TValue>.CreateIntf(
  const AEnumerable: IEnumerable<TKeyValuePair<TKey, TValue>>;
  const AKeyType: IType<TKey>; const AValueType: IType<TValue>);
begin
  { Call the higher constructor }
  Create(TEnexAssociativeWrapCollection<TKey, TValue>.Create(AEnumerable, AKeyType, AValueType));

  { Mark for deletion }
  FDeleteEnum := true;
end;

destructor TEnexAssociativeDistinctByKeysCollection<TKey, TValue>.Destroy;
begin
  { Delete the enumerable if required }
  ReleaseObject(FEnum, FDeleteEnum);

  inherited;
end;

function TEnexAssociativeDistinctByKeysCollection<TKey, TValue>.GetEnumerator: IEnumerator<TKeyValuePair<TKey, TValue>>;
begin
  { Create an enumerator }
  Result := TEnumerator.Create(Self);
end;

{ TEnexAssociativeDistinctByKeysCollection<TKey, TValue>.TEnumerator }

constructor TEnexAssociativeDistinctByKeysCollection<TKey, TValue>.TEnumerator.Create(
  const AEnum: TEnexAssociativeDistinctByKeysCollection<TKey, TValue>);
begin
  { Initialize }
  FEnum := AEnum;
  KeepObjectAlive(FEnum);

  FIter := AEnum.FEnum.GetEnumerator();

  { Create an internal set }
  FSet := THashSet<TKey>.Create(TSuppressedWrapperType<TKey>.Create(AEnum.FEnum.KeyType));
end;

destructor TEnexAssociativeDistinctByKeysCollection<TKey, TValue>.TEnumerator.Destroy;
begin
  ReleaseObject(FEnum);
  inherited;
end;

function TEnexAssociativeDistinctByKeysCollection<TKey, TValue>.TEnumerator.GetCurrent: TKeyValuePair<TKey, TValue>;
begin
  { Get from sub-enum }
  Result := FIter.Current;
end;

function TEnexAssociativeDistinctByKeysCollection<TKey, TValue>.TEnumerator.MoveNext: Boolean;
begin
  while True do
  begin
    { Iterate }
    Result := FIter.MoveNext;

    if not Result then
      Exit;

    { If the item is distinct, add it to set and continue }
    if not FSet.Contains(FIter.Current.Key) then
    begin
      FSet.Add(FIter.Current.Key);
      Exit;
    end;
  end;
end;


{ TEnexAssociativeDistinctByValuesCollection<TKey, TValue> }

constructor TEnexAssociativeDistinctByValuesCollection<TKey, TValue>.Create(
  const AEnumerable: TEnexAssociativeCollection<TKey, TValue>);
begin
  { Check arguments }
  if not Assigned(AEnumerable) then
    ExceptionHelper.Throw_ArgumentNilError('AEnumerable');

  { Install types }
  InstallTypes(AEnumerable.KeyType, AEnumerable.ValueType);

  { Assign internals }
  FEnum := AEnumerable;
  KeepObjectAlive(FEnum);

  FDeleteEnum := false;
end;

constructor TEnexAssociativeDistinctByValuesCollection<TKey, TValue>.CreateIntf(
  const AEnumerable: IEnumerable<TKeyValuePair<TKey, TValue>>;
  const AKeyType: IType<TKey>; const AValueType: IType<TValue>);
begin
  { Call the higher constructor }
  Create(TEnexAssociativeWrapCollection<TKey, TValue>.Create(AEnumerable, AKeyType, AValueType));

  { Mark for deletion }
  FDeleteEnum := true;
end;

destructor TEnexAssociativeDistinctByValuesCollection<TKey, TValue>.Destroy;
begin
  { Delete the enumerable if required }
  ReleaseObject(FEnum, FDeleteEnum);

  inherited;
end;

function TEnexAssociativeDistinctByValuesCollection<TKey, TValue>.GetEnumerator: IEnumerator<TKeyValuePair<TKey, TValue>>;
begin
  { Create an enumerator }
  Result := TEnumerator.Create(Self);
end;

{ TEnexAssociativeDistinctByValuesCollection<TKey, TValue>.TEnumerator }

constructor TEnexAssociativeDistinctByValuesCollection<TKey, TValue>.TEnumerator.Create(
  const AEnum: TEnexAssociativeDistinctByValuesCollection<TKey, TValue>);
begin
  { Initialize }
  FEnum := AEnum;
  KeepObjectAlive(FEnum);

  FIter := AEnum.FEnum.GetEnumerator();

  { Create an internal set }
  FSet := THashSet<TValue>.Create(TSuppressedWrapperType<TValue>.Create(AEnum.FEnum.ValueType));
end;

destructor TEnexAssociativeDistinctByValuesCollection<TKey, TValue>.TEnumerator.Destroy;
begin
  ReleaseObject(FEnum);
  inherited;
end;

function TEnexAssociativeDistinctByValuesCollection<TKey, TValue>.TEnumerator.GetCurrent: TKeyValuePair<TKey, TValue>;
begin
  { Get from sub-enum }
  Result := FIter.Current;
end;

function TEnexAssociativeDistinctByValuesCollection<TKey, TValue>.TEnumerator.MoveNext: Boolean;
begin
  while True do
  begin
    { Iterate }
    Result := FIter.MoveNext;

    if not Result then
      Exit;

    { If the item is distinct, add it to set and continue }
    if not FSet.Contains(FIter.Current.Value) then
    begin
      FSet.Add(FIter.Current.Value);
      Exit;
    end;
  end;
end;

{ TEnexExtOps<T> }

function TEnexExtOps<T>.Cast<TOut>: IEnexCollection<TOut>;
begin
  { Call super-function }
  Result := Cast<TOut>(TType<TOut>.Default);
end;

function TEnexExtOps<T>.Cast<TOut>(const AType: IType<TOut>): IEnexCollection<TOut>;
begin
  { Check arguments }
  if not Assigned(AType) then
    ExceptionHelper.Throw_ArgumentNilError('AType');

  { Create a new Enex collection }
  Result := TEnexCastCollection<T, TOut>.Create(FInstance, AType);
end;

function TEnexExtOps<T>.Select<TOut>(const ASelector: TFunc<T, TOut>): IEnexCollection<TOut>;
begin
  { With default type support }
  Result := Select<TOut>(ASelector, TType<TOut>.Default);
end;

function TEnexExtOps<T>.Select<TOut>(const ASelector: TFunc<T, TOut>; const AType: IType<TOut>): IEnexCollection<TOut>;
begin
  { Check arguments }
  if not Assigned(ASelector) then
    ExceptionHelper.Throw_ArgumentNilError('ASelector');

  if not Assigned(AType) then
    ExceptionHelper.Throw_ArgumentNilError('AType');

  { Create a new Enex collection }
  Result := TEnexSelectCollection<T, TOut>.Create(FInstance, ASelector, AType);
end;

end.
