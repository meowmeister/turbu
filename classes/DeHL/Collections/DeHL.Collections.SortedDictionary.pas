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
unit DeHL.Collections.SortedDictionary;
interface
uses SysUtils,
     DeHL.Base,
     DeHL.Types,
     DeHL.Exceptions,
     DeHL.Arrays,
     DeHL.StrConsts,
     DeHL.Serialization,
     DeHL.KeyValuePair,
     DeHL.Collections.Base;

type
  { Generic Dictionary }
  TSortedDictionary<TKey, TValue> = class(TEnexAssociativeCollection<TKey, TValue>, IDictionary<TKey, TValue>)
  private
  type
    TBalanceAct = (baStart, baLeft, baRight, baLoop, baEnd);

    { An internal node class }
    TNode = class
    private
      FKey: TKey;
      FValue: TValue;

      FParent,
       FLeft, FRight: TNode;

      FBalance: ShortInt;
    end;

    { Generic Dictionary Pairs Enumerator }
    TPairEnumerator = class(TEnumerator<TKeyValuePair<TKey,TValue>>)
    private
      FVer        : Cardinal;
      FDict       : TSortedDictionary<TKey, TValue>;
      FNext       : TNode;
      FValue      : TKeyValuePair<TKey,TValue>;

    public
      { Constructor }
      constructor Create(const ADict: TSortedDictionary<TKey, TValue>);

      { Destructor }
      destructor Destroy(); override;

      function GetCurrent(): TKeyValuePair<TKey,TValue>; override;
      function MoveNext(): Boolean; override;
    end;

    { Generic Dictionary Keys Enumerator }
    TKeyEnumerator = class(TEnumerator<TKey>)
    private
      FVer        : Cardinal;
      FDict       : TSortedDictionary<TKey, TValue>;
      FNext       : TNode;
      FValue      : TKey;

    public
      { Constructor }
      constructor Create(const ADict: TSortedDictionary<TKey, TValue>);

      { Destructor }
      destructor Destroy(); override;

      function GetCurrent(): TKey; override;
      function MoveNext(): Boolean; override;
    end;

    { Generic Dictionary Values Enumerator }
    TValueEnumerator = class(TEnumerator<TValue>)
    private
      FVer        : Cardinal;
      FDict       : TSortedDictionary<TKey, TValue>;
      FNext       : TNode;
      FValue      : TValue;

    public
      { Constructor }
      constructor Create(const ADict: TSortedDictionary<TKey, TValue>);

      { Destructor }
      destructor Destroy(); override;

      function GetCurrent(): TValue; override;
      function MoveNext(): Boolean; override;
    end;

    { Generic Dictionary Keys Collection }
    TKeyCollection = class(TEnexCollection<TKey>)
    private
      FDict       : TSortedDictionary<TKey, TValue>;

    protected
      { Hidden }
      function GetCount(): Cardinal; override;

    public
      { Constructor }
      constructor Create(const ADict: TSortedDictionary<TKey, TValue>);

      { Destructor }
      destructor Destroy(); override;

      { Property }
      property Count: Cardinal read GetCount;

      { IEnumerable/ ICollection support }
      function GetEnumerator(): IEnumerator<TKey>; override;

      { Copy-To }
      procedure CopyTo(var AArray: array of TKey); overload; override;
      procedure CopyTo(var AArray: array of TKey; const StartIndex: Cardinal); overload; override;
    end;

    { Generic Dictionary Values Collection }
    TValueCollection = class(TEnexCollection<TValue>)
    private
      FDict       : TSortedDictionary<TKey, TValue>;

    protected
      { Hidden }
      function GetCount: Cardinal; override;

    public
      { Constructor }
      constructor Create(const ADict: TSortedDictionary<TKey, TValue>);

      { Destructor }
      destructor Destroy(); override;

      { Property }
      property Count: Cardinal read GetCount;

      { IEnumerable/ ICollection support }
      function GetEnumerator(): IEnumerator<TValue>; override;

      { Copy-To }
      procedure CopyTo(var AArray: array of TValue); overload; override;
      procedure CopyTo(var AArray: array of TValue; const StartIndex: Cardinal); overload; override;
    end;

  var
    FCount       : Cardinal;
    FVer         : Cardinal;
    FRoot        : TNode;
    FSignFix     : Integer;
    FKeyCollection: IEnexCollection<TKey>;
    FValueCollection: IEnexCollection<TValue>;

    { Some internals }
    function FindNodeWithKey(const AKey: TKey): TNode;
    function FindLeftMostNode(): TNode;
    function FindRightMostNode(): TNode;
    function WalkToTheRight(const ANode: TNode): TNode;

    { ... }
    function MakeNode(const AKey: TKey; const AValue: TValue; const ARoot: TNode): TNode;
    procedure RecursiveClear(const ANode: TNode);
    procedure ReBalanceSubTreeOnInsert(const ANode: TNode);
    function Insert(const AKey: TKey; const AValue: TValue; const ChangeOrFail: Boolean): Boolean;

    { Removal }
    procedure BalanceTreesAfterRemoval(const ANode: TNode);
  protected
    { Serialization overrides }
    procedure StartSerializing(const AData: TSerializationData); override;
    procedure StartDeserializing(const AData: TDeserializationData); override;
    procedure DeserializePair(const AKey: TKey; const AValue: TValue); override;

    { Hidden }
    function GetCount(): Cardinal; override;

    { Key getter and setter }
    function GetItem(const Key: TKey): TValue;
    procedure SetItem(const Key: TKey; const Value: TValue);

  public
    { Constructors }
    constructor Create(const Ascending: Boolean = true); overload;
    constructor Create(const AEnumerable: IEnumerable<TKeyValuePair<TKey, TValue>>; const Ascending: Boolean = true); overload;
    constructor Create(const AArray: array of TKeyValuePair<TKey, TValue>; const Ascending: Boolean = true); overload;
    constructor Create(const AArray: TDynamicArray<TKeyValuePair<TKey, TValue>>; const Ascending: Boolean = true); overload;
    constructor Create(const AArray: TFixedArray<TKeyValuePair<TKey, TValue>>; const Ascending: Boolean = true); overload;

    constructor Create(const AKeyType: IType<TKey>; const AValueType: IType<TValue>; const Ascending: Boolean = true); overload;
    constructor Create(const AKeyType: IType<TKey>; const AValueType: IType<TValue>;
          const AEnumerable: IEnumerable<TKeyValuePair<TKey, TValue>>; const Ascending: Boolean = true); overload;
    constructor Create(const AKeyType: IType<TKey>; const AValueType: IType<TValue>;
          const AArray: array of TKeyValuePair<TKey,TValue>; const Ascending: Boolean = true); overload;
    constructor Create(const AKeyType: IType<TKey>; const AValueType: IType<TValue>;
          const AArray: TDynamicArray<TKeyValuePair<TKey,TValue>>; const Ascending: Boolean = true); overload;
    constructor Create(const AKeyType: IType<TKey>; const AValueType: IType<TValue>;
          const AArray: TFixedArray<TKeyValuePair<TKey,TValue>>; const Ascending: Boolean = true); overload;

    { Destructor }
    destructor Destroy(); override;

    {  Modification }
    procedure Clear();

    procedure Add(const APair: TKeyValuePair<TKey,TValue>); overload;
    procedure Add(const AKey: TKey; const AValue: TValue); overload;

    procedure Remove(const AKey: TKey); overload;

    { Lookup }
    function ContainsKey(const AKey: TKey): Boolean;
    function ContainsValue(const AValue: TValue): Boolean;
    function TryGetValue(const AKey: TKey; out FoundValue: TValue): Boolean;

    { Properties }
    property Items[const Key: TKey]: TValue read GetItem write SetItem; default;
    property Count: Cardinal read FCount;

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

    { Enex - Associative collection }
    function MaxKey(): TKey; override;
    function MinKey(): TKey; override;

    { Enex - selectors }
    function SelectKeys(): IEnexCollection<TKey>; override;
    function SelectValues(): IEnexCollection<TValue>; override;
  end;

  { The object variant }
  TObjectSortedDictionary<TKey, TValue: class> = class sealed(TSortedDictionary<TKey, TValue>)
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

{ TSortedDictionary<TKey, TValue> }

procedure TSortedDictionary<TKey, TValue>.Add(const APair: TKeyValuePair<TKey, TValue>);
begin
  { Insert the pair }
  if not Insert(APair.Key, APair.Value, false) then
    ExceptionHelper.Throw_DuplicateKeyError('AKey');
end;

procedure TSortedDictionary<TKey, TValue>.Add(const AKey: TKey; const AValue: TValue);
begin
  { Insert the pair }
  if not Insert(AKey, AValue, false) then
    ExceptionHelper.Throw_DuplicateKeyError('AKey');
end;

procedure TSortedDictionary<TKey, TValue>.BalanceTreesAfterRemoval(const ANode: TNode);
var
  CurrentAct: TBalanceAct;
  LNode, XNode,
    SNode, WNode,
      YNode: TNode;
begin
  { Initiliaze ... }
  CurrentAct := TBalanceAct.baStart;
  LNode := ANode;

  { Continue looping until end is declared }
  while CurrentAct <> TBalanceAct.baEnd do
  begin
    case CurrentAct of

      { START MODE }
      TBalanceAct.baStart:
      begin
        if LNode.FRight = nil then
        begin
          { Exclude myself! }
          if LNode.FLeft <> nil then
            LNode.FLeft.FParent := LNode.FParent;

          { I'm root! nothing to do here }
          if LNode.FParent = nil then
          begin
            FRoot := LNode.FLeft;

            { DONE! }
            CurrentAct := TBalanceAct.baEnd;
            continue;
          end;

          { ... }
          if LNode = LNode.FParent.FLeft then
          begin
            LNode.FParent.FLeft := LNode.FLeft;
            YNode := LNode.FParent;
          end else
          begin
            LNode.FParent.FRight := LNode.FLeft;
            YNode := LNode.FParent;

            { RIGHT! }
            CurrentAct := TBalanceAct.baRight;
            continue;
          end;
        end else if LNode.FRight.FLeft = nil then
        begin
          { Case 1, RIGHT, NO LEFT }
          if LNode.FLeft <> nil then
          begin
            LNode.FLeft.FParent := LNode.FRight;
            LNode.FRight.FLeft := LNode.FLeft;
          end;

          LNode.FRight.FBalance := LNode.FBalance;
          LNode.FRight.FParent := LNode.FParent;

          if LNode.FParent = nil then
            FRoot := LNode.FRight
          else
          begin
            if LNode = LNode.FParent.FLeft then
              LNode.FParent.FLeft := LNode.FRight
            else
              LNode.FParent.FRight := LNode.FRight;
          end;

          YNode := LNode.FRight;

          { RIGHT! }
          CurrentAct := TBalanceAct.baRight;
          continue;
        end else
        begin
          { Case 3: RIGHT+LEFT }
          SNode := LNode.FRight.FLeft;

          while SNode.FLeft <> nil do
            SNode := SNode.FLeft;

          if LNode.FLeft <> nil then
          begin
            LNode.FLeft.FParent := SNode;
            SNode.FLeft := LNode.FLeft;
          end;

          SNode.FParent.FLeft := SNode.FRight;

          if SNode.FRight <> nil then
            SNode.FRight.FParent := SNode.FParent;

          LNode.FRight.FParent := SNode;
          SNode.FRight := LNode.FRight;

          YNode := SNode.FParent;

          SNode.FBalance := LNode.FBalance;
          SNode.FParent := LNode.FParent;

          if LNode.FParent = nil then
            FRoot := SNode
          else
          begin
            if LNode = LNode.FParent.FLeft then
              LNode.FParent.FLeft := SNode
            else
              LNode.FParent.FRight := SNode;
          end;
        end;

        { LEFT! }
        CurrentAct := TBalanceAct.baLeft;
        continue;
      end; { baStart }

      { LEFT BALANCING MODE }
      TBalanceAct.baLeft:
      begin
        Inc(YNode.FBalance);

        if YNode.FBalance = 1 then
        begin
          { DONE! }
          CurrentAct := TBalanceAct.baEnd;
          continue;
        end
        else if YNode.FBalance = 2 then
        begin
          XNode := YNode.FRight;

          if XNode.FBalance = -1 then
          begin
            WNode := XNode.FLeft;
            WNode.FParent := YNode.FParent;

            if YNode.FParent = nil then
              FRoot := WNode
            else
            begin
              if YNode.FParent.FLeft = YNode then
                YNode.FParent.FLeft := WNode
              else
                YNode.FParent.FRight := WNode;
            end;

            XNode.FLeft := WNode.FRight;

            if XNode.FLeft <> nil then
              XNode.FLeft.FParent := XNode;

            YNode.FRight := WNode.FLeft;

            if YNode.FRight <> nil then
              YNode.FRight.FParent := YNode;

            WNode.FRight := XNode;
            WNode.FLeft := YNode;

            XNode.FParent := WNode;
            YNode.FParent := WNode;

            if WNode.FBalance = 1 then
            begin
              XNode.FBalance := 0;
              YNode.FBalance := -1;
            end else if WNode.FBalance = 0 then
            begin
              XNode.FBalance := 0;
              YNode.FBalance := 0;
            end else
            begin
              XNode.FBalance := 1;
              YNode.FBalance := 0;
            end;

            WNode.FBalance := 0;
            YNode := WNode;
          end else
          begin
            XNode.FParent := YNode.FParent;

            if YNode.FParent <> nil then
            begin
              if YNode.FParent.FLeft = YNode then
                YNode.FParent.FLeft := XNode
              else
                YNode.FParent.FRight := XNode;
            end else
              FRoot := XNode;

            YNode.FRight := XNode.FLeft;

            if YNode.FRight <> nil then
              YNode.FRight.FParent := YNode;

            XNode.FLeft := YNode;
            YNode.FParent := XNode;

            if XNode.FBalance = 0 then
            begin
              XNode.FBalance := -1;
              YNode.FBalance := 1;

              { DONE! }
              CurrentAct := TBalanceAct.baEnd;
              continue;
            end else
            begin
              XNode.FBalance := 0;
              YNode.FBalance := 0;

              YNode := XNode;
            end;
          end;
        end;

        { LOOP! }
        CurrentAct := TBalanceAct.baLoop;
        continue;
      end; { baLeft }

      { RIGHT BALANCING MODE }
      TBalanceAct.baRight:
      begin
        Dec(YNode.FBalance);

        if YNode.FBalance = -1 then
        begin
          { DONE! }
          CurrentAct := TBalanceAct.baEnd;
          continue;
        end
        else if YNode.FBalance = -2 then
        begin
          XNode := YNode.FLeft;

          if XNode.FBalance = 1 then
          begin
            WNode := XNode.FRight;
            WNode.FParent := YNode.FParent;

            if YNode.FParent = nil then
              FRoot := WNode
            else
            begin
              if YNode.FParent.FLeft = YNode then
                YNode.FParent.FLeft := WNode
              else
                YNode.FParent.FRight := WNode;
            end;

            XNode.FRight := WNode.FLeft;

            if XNode.FRight <> nil then
              XNode.FRight.FParent := XNode;

            YNode.FLeft := WNode.FRight;

            if YNode.FLeft <> nil then
              YNode.FLeft.FParent := YNode;

            WNode.FLeft := XNode;
            WNode.FRight := YNode;

            XNode.FParent := WNode;
            YNode.FParent := WNode;

            if WNode.FBalance = -1 then
            begin
              XNode.FBalance := 0;
              YNode.FBalance := 1;
            end else if WNode.FBalance = 0 then
            begin
              XNode.FBalance := 0;
              YNode.FBalance := 0;
            end else
            begin
              XNode.FBalance := -1;
              YNode.FBalance := 0;
            end;

            WNode.FBalance := 0;
            YNode := WNode;
          end else
          begin
            XNode.FParent := YNode.FParent;

            if YNode.FParent <> nil then
            begin
              if YNode.FParent.FLeft = YNode then
                YNode.FParent.FLeft := XNode
              else
                YNode.FParent.FRight := XNode
            end else
              FRoot := XNode;

            YNode.FLeft := XNode.FRight;

            if YNode.FLeft <> nil then
              YNode.FLeft.FParent := YNode;

            XNode.FRight := YNode;
            YNode.FParent := XNode;

            if XNode.FBalance = 0 then
            begin
              XNode.FBalance := 1;
              YNode.FBalance := -1;

              { END! }
              CurrentAct := TBalanceAct.baEnd;
              continue;
            end else
            begin
              XNode.FBalance := 0;
              YNode.FBalance := 0;

              YNode := XNode;
            end;
          end;
        end;

        { LOOP! }
        CurrentAct := TBalanceAct.baLoop;
        continue;
      end; { baRight }

      TBalanceAct.baLoop:
      begin
        { Verify continuation }
        if YNode.FParent <> nil then
        begin
          if YNode = YNode.FParent.FLeft then
          begin
            YNode := YNode.FParent;

            { LEFT! }
            CurrentAct := TBalanceAct.baLeft;
            continue;
          end;

          YNode := YNode.FParent;

          { RIGHT! }
          CurrentAct := TBalanceAct.baRight;
          continue;
        end;

        { END! }
        CurrentAct := TBalanceAct.baEnd;
        continue;
      end;
    end; { Case }
  end; { While }
end;

procedure TSortedDictionary<TKey, TValue>.Clear;
begin
  if FRoot <> nil then
  begin
    RecursiveClear(FRoot);
    FRoot := nil;

    { Update markers }
    Inc(FVer);
    FCount := 0;
  end;
end;

function TSortedDictionary<TKey, TValue>.ContainsKey(const AKey: TKey): Boolean;
begin
  Result := FindNodeWithKey(AKey) <> nil;
end;

function TSortedDictionary<TKey, TValue>.ContainsValue(const AValue: TValue): Boolean;
var
  LNode: TNode;
begin
  { Find the left-most node }
  LNode := FindLeftMostNode();

  while (LNode <> nil) do
  begin
    { Verify existance }
    if ValueType.AreEqual(LNode.FValue, AValue) then
      Exit(true);

    { Navigate further in the tree }
    LNode := WalkToTheRight(LNode);
  end;

  Exit(false);
end;

procedure TSortedDictionary<TKey, TValue>.CopyTo(var AArray: array of TKeyValuePair<TKey, TValue>);
begin
  { Call the more generic function }
  CopyTo(AArray, 0);
end;

procedure TSortedDictionary<TKey, TValue>.CopyTo(var AArray: array of TKeyValuePair<TKey, TValue>; const StartIndex: Cardinal);
var
  X: Integer;
  LNode: TNode;
begin
  { Check for indexes }
  if StartIndex >= Cardinal(Length(AArray)) then
    ExceptionHelper.Throw_ArgumentOutOfRangeError('StartIndex');

  if (Cardinal(Length(AArray)) - StartIndex) < FCount then
     ExceptionHelper.Throw_ArgumentOutOfSpaceError('AArray');

  X := StartIndex;

  { Find the left-most node }
  LNode := FindLeftMostNode();

  while (LNode <> nil) do
  begin
    { Get the key }
    AArray[X] := TKeyValuePair<TKey, TValue>.Create(LNode.FKey, LNode.FValue);

    { Navigate further in the tree }
    LNode := WalkToTheRight(LNode);

    { Increment the index }
    Inc(X);
  end;
end;

constructor TSortedDictionary<TKey, TValue>.Create(const Ascending: Boolean);
begin
  Create(TType<TKey>.Default, TType<TValue>.Default, Ascending);
end;

constructor TSortedDictionary<TKey, TValue>.Create(const AEnumerable: IEnumerable<TKeyValuePair<TKey, TValue>>;
  const Ascending: Boolean);
begin
  Create(TType<TKey>.Default, TType<TValue>.Default, AEnumerable, Ascending);
end;

constructor TSortedDictionary<TKey, TValue>.Create(const AKeyType: IType<TKey>; const AValueType: IType<TValue>;
  const AEnumerable: IEnumerable<TKeyValuePair<TKey, TValue>>; const Ascending: Boolean);
var
  V: TKeyValuePair<TKey, TValue>;
begin
  { Call upper constructor }
  Create(AKeyType, AValueType, Ascending);

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

constructor TSortedDictionary<TKey, TValue>.Create(const AKeyType: IType<TKey>;
  const AValueType: IType<TValue>; const Ascending: Boolean);
begin
  { Initialize instance }
  if (AKeyType = nil) then
     ExceptionHelper.Throw_ArgumentNilError('AKeyType');

  if (AValueType = nil) then
     ExceptionHelper.Throw_ArgumentNilError('AValueType');

  { Install types }
  InstallTypes(AKeyType, AValueType);

  FKeyCollection := TKeyCollection.Create(Self);
  FValueCollection := TValueCollection.Create(Self);

  FVer := 0;
  FCount := 0;

  if Ascending then
    FSignFix := 1
  else
    FSignFix := -1;
end;

procedure TSortedDictionary<TKey, TValue>.DeserializePair(const AKey: TKey; const AValue: TValue);
begin
  { Simple as hell ... }
  Add(AKey, AValue);
end;

destructor TSortedDictionary<TKey, TValue>.Destroy;
begin
  { Clear first }
  Clear();

  inherited;
end;

function TSortedDictionary<TKey, TValue>.FindLeftMostNode: TNode;
begin
  { Start with root }
  Result := FRoot;

  { And go to maximum left }
  if Result <> nil then
  begin
    while Result.FLeft <> nil do
      Result := Result.FLeft;
  end;
end;

function TSortedDictionary<TKey, TValue>.FindNodeWithKey(const AKey: TKey): TNode;
var
  LNode: TNode;
  Compare: Integer;
  HACK: IInterface;
begin
  { Get root }
  LNode := FRoot;

  while LNode <> nil do
  begin
	  Compare := KeyType.Compare(AKey, LNode.FKey) * FSignFix;

    { Navigate left, right or find! }
    if Compare < 0 then
      LNode := LNode.FLeft
    else if Compare > 0 then
      LNode := LNode.FRight
    else
      Exit(LNode);
  end;

  { Did not find anything ... }
  Result := nil;
end;

function TSortedDictionary<TKey, TValue>.FindRightMostNode: TNode;
begin
  { Start with root }
  Result := FRoot;

  { And go to maximum left }
  if Result <> nil then
  begin
    while Result.FRight <> nil do
      Result := Result.FRight;
  end;
end;

function TSortedDictionary<TKey, TValue>.GetCount: Cardinal;
begin
  Result := FCount;
end;

function TSortedDictionary<TKey, TValue>.GetEnumerator: IEnumerator<TKeyValuePair<TKey, TValue>>;
begin
  Result := TPairEnumerator.Create(Self);
end;

function TSortedDictionary<TKey, TValue>.GetItem(const Key: TKey): TValue;
begin
  if not TryGetValue(Key, Result) then
    ExceptionHelper.Throw_KeyNotFoundError(KeyType.GetString(Key));
end;

function TSortedDictionary<TKey, TValue>.Insert(const AKey: TKey; const AValue: TValue; const ChangeOrFail: Boolean): Boolean;
var
  LNode: TNode;
  Compare: Integer;
begin
  { First one get special treatment! }
  if FRoot = nil then
  begin
    FRoot := MakeNode(AKey, AValue, nil);

    { Increase markers }
    Inc(FCount);
    Inc(FVer);

    { [ADDED NEW] Exit function }
    Exit(true);
  end;

  { Get root }
  LNode := FRoot;

  while true do
  begin
	  Compare := KeyType.Compare(AKey, LNode.FKey) * FSignFix;

    if Compare < 0 then
    begin
      if LNode.FLeft <> nil then
        LNode := LNode.FLeft
      else
      begin
        { Create a new node }
        LNode.FLeft := MakeNode(AKey, AValue, LNode);
        Dec(LNode.FBalance);

        { [ADDED NEW] Exit function! }
        break;
      end;
    end else if Compare > 0 then
    begin
      if LNode.FRight <> nil then
        LNode := LNode.FRight
      else
      begin
        LNode.FRight := MakeNode(AKey, AValue, LNode);
        Inc(LNode.FBalance);

        { [ADDED NEW] Exit function! }
        break;
      end;
    end else
    begin
      { Found a node with the same key. Check what to do next }
      if not ChangeOrFail then
        Exit(false);

      { Cleanup the value if required }
      if ValueType.Management = tmManual then
        ValueType.Cleanup(LNode.FValue);

      { Change the node value }
      LNode.FValue := AValue;

      { Increase markers }
      Inc(FVer);

      { [CHANGED OLD] Exit function }
      Exit(true);
    end;
  end;

  { Rebalance the tree }
  ReBalanceSubTreeOnInsert(LNode);

  Inc(FCount);
  Inc(FVer);

  Result := true;
end;

function TSortedDictionary<TKey, TValue>.KeyHasValue(const AKey: TKey; const AValue: TValue): Boolean;
var
  LValue: TValue;
begin
  Result := TryGetValue(AKey, LValue) and ValueType.AreEqual(LValue, AValue);
end;

function TSortedDictionary<TKey, TValue>.MakeNode(const AKey: TKey; const AValue: TValue; const ARoot: TNode): TNode;
begin
  Result := TNode.Create();
  Result.FKey := AKey;
  Result.FValue := AValue;
  Result.FParent := ARoot;
end;

function TSortedDictionary<TKey, TValue>.MaxKey: TKey;
begin
  { Check there are elements in the set }
  if FRoot = nil then
    ExceptionHelper.Throw_CollectionEmptyError();

  if FSignFix = 1 then
    Result := FindRightMostNode().FKey
  else
    Result := FindLeftMostNode().FKey;
end;

function TSortedDictionary<TKey, TValue>.MinKey: TKey;
begin
  { Check there are elements in the set }
  if FRoot = nil then
    ExceptionHelper.Throw_CollectionEmptyError();

  if FSignFix = 1 then
    Result := FindLeftMostNode().FKey
  else
    Result := FindRightMostNode().FKey;
end;

procedure TSortedDictionary<TKey, TValue>.ReBalanceSubTreeOnInsert(const ANode: TNode);
var
  LNode, XNode, WNode: TNode;
  Compare: Integer;
begin
  (*
    DISCLAIMER: I HAVE LITTLE TO ABSOLUTELY NO IDEA HOW THIS SPAGETTI WORKS!
    DO NOT BLAME ME :D (Alex).
  *)

  LNode := ANode;

  { Re-balancing the tree! }
  while ((LNode.FBalance <> 0) and (LNode.FParent <> nil)) do
  begin
    if (LNode.FParent.FLeft = LNode) then
      Dec(LNode.FParent.FBalance)
    else
      Inc(LNode.FParent.FBalance);

    { Move up }
    LNode := LNode.FParent;

    if (LNode.FBalance = -2) then
    begin
      XNode := LNode.FLeft;

      if (XNode.FBalance = -1) then
      begin
        XNode.FParent := LNode.FParent;

        if (LNode.FParent = nil) then
          FRoot := XNode
        else
        begin
          if (LNode.FParent.FLeft = LNode) then
            LNode.FParent.FLeft := XNode
          else
            LNode.FParent.FRight := XNode;
        end;

        LNode.FLeft := XNode.FRight;

        if LNode.FLeft <> nil then
          LNode.FLeft.FParent := LNode;

        XNode.FRight := LNode;
        LNode.FParent := XNode;

        XNode.FBalance := 0;
        LNode.FBalance := 0;
      end else
      begin
        WNode := XNode.FRight;
        WNode.FParent := LNode.FParent;

        if LNode.FParent = nil then
          FRoot := WNode
        else
        begin
          if LNode.FParent.FLeft = LNode then
            LNode.FParent.FLeft := WNode
          else
            LNode.FParent.FRight := WNode;
        end;

        XNode.FRight := WNode.FLeft;

        if XNode.FRight <> nil then
          XNode.FRight.FParent := XNode;

        LNode.FLeft := WNode.FRight;

        if LNode.FLeft <> nil then
          LNode.FLeft.FParent := LNode;

        WNode.FLeft := XNode;
        WNode.FRight := LNode;

        XNode.FParent := WNode;
        LNode.FParent := WNode;

        { Apply proper balancing }
        if WNode.FBalance = -1 then
        begin
          XNode.FBalance := 0;
          LNode.FBalance := 1;
        end else if WNode.FBalance = 0 then
        begin
          XNode.FBalance := 0;
          LNode.FBalance := 0;
        end else
        begin
          XNode.FBalance := -1;
          LNode.FBalance := 0;
        end;

        WNode.FBalance := 0;
      end;

      break;
    end else if LNode.FBalance = 2 then
    begin
      XNode := LNode.FRight;

      if XNode.FBalance = 1 then
      begin
        XNode.FParent := LNode.FParent;

        if LNode.FParent = nil then
          FRoot := XNode
        else
        begin
          if LNode.FParent.FLeft = LNode then
            LNode.FParent.FLeft := XNode
          else
            LNode.FParent.FRight := XNode;
        end;

        LNode.FRight := XNode.FLeft;

        if LNode.FRight <> nil then
          LNode.FRight.FParent := LNode;

        XNode.FLeft := LNode;
        LNode.FParent := XNode;

        XNode.FBalance := 0;
        LNode.FBalance := 0;
      end else
      begin
        WNode := XNode.FLeft;
        WNode.FParent := LNode.FParent;

        if LNode.FParent = nil then
          FRoot := WNode
        else
        begin
          if LNode.FParent.FLeft = LNode then
            LNode.FParent.FLeft := WNode
          else
            LNode.FParent.FRight := WNode;
        end;

        XNode.FLeft := WNode.FRight;

        if XNode.FLeft <> nil then
          XNode.FLeft.FParent := XNode;

        LNode.FRight := WNode.FLeft;

        if LNode.FRight <> nil then
          LNode.FRight.FParent := LNode;

        WNode.FRight := XNode;
        WNode.FLeft := LNode;

        XNode.FParent := WNode;
        LNode.FParent := WNode;

        if WNode.FBalance = 1 then
        begin
          XNode.FBalance := 0;
          LNode.FBalance := -1;
        end else if WNode.FBalance = 0 then
        begin
          XNode.FBalance := 0;
          LNode.FBalance := 0;
        end else
        begin
          XNode.FBalance := 1;
          LNode.FBalance := 0;
        end;

        WNode.FBalance := 0;
      end;

      break;
    end;
  end;

end;

procedure TSortedDictionary<TKey, TValue>.Remove(const AKey: TKey);
var
  LNode: TNode;

begin
  { Get root }
  LNode := FindNodeWithKey(AKey);

  { Remove and rebalance the tree accordingly }
  if LNode = nil then
    Exit;

  { .. Do da dew! }
  BalanceTreesAfterRemoval(LNode);

  { Kill the stored value }
  if ValueType.Management = tmManual then
    ValueType.Cleanup(LNode.FValue);

  { Kill the node }
  LNode.Free;

  Dec(FCount);
  Inc(FVer);
end;

procedure TSortedDictionary<TKey, TValue>.RecursiveClear(const ANode: TNode);
begin
  if ANode.FLeft <> nil then
    RecursiveClear(ANode.FLeft);

  if ANode.FRight <> nil then
    RecursiveClear(ANode.FRight);

  { Cleanup for Key/Value }
  if KeyType.Management = tmManual then
    KeyType.Cleanup(ANode.FKey);

  if ValueType.Management = tmManual then
    ValueType.Cleanup(ANode.FValue);

  { Finally, free the node itself }
  ANode.Free;
end;

function TSortedDictionary<TKey, TValue>.SelectKeys: IEnexCollection<TKey>;
begin
  Result := Keys;
end;

function TSortedDictionary<TKey, TValue>.SelectValues: IEnexCollection<TValue>;
begin
  Result := Values;
end;

procedure TSortedDictionary<TKey, TValue>.SetItem(const Key: TKey; const Value: TValue);
begin
  { Allow inserting and adding values }
  Insert(Key, Value, true);
end;

procedure TSortedDictionary<TKey, TValue>.StartDeserializing(const AData: TDeserializationData);
var
  LAsc: Boolean;
begin
  AData.GetValue(SSerAscendingKeys, LAsc);

  { Call the constructor in this instance to initialize myself first }
  Create(LAsc);
end;

procedure TSortedDictionary<TKey, TValue>.StartSerializing(const AData: TSerializationData);
begin
  { Write the ascending sign }
  AData.AddValue(SSerAscendingKeys, (FSignFix = 1));
end;

function TSortedDictionary<TKey, TValue>.TryGetValue(const AKey: TKey; out FoundValue: TValue): Boolean;
var
  ResultNode: TNode;
begin
  ResultNode := FindNodeWithKey(AKey);

  if ResultNode <> nil then
  begin
    FoundValue := ResultNode.FValue;
    Exit(true);
  end;

  { Default }
  FoundValue := default(TValue);
  Exit(false);
end;

function TSortedDictionary<TKey, TValue>.ValueForKey(const AKey: TKey): TValue;
begin
  Result := GetItem(AKey);
end;

function TSortedDictionary<TKey, TValue>.WalkToTheRight(const ANode: TNode): TNode;
begin
  Result := ANode;

  if Result = nil then
    Exit;

  { Navigate further in the tree }
  if Result.FRight = nil then
  begin
    while ((Result.FParent <> nil) and (Result = Result.FParent.FRight)) do
      Result := Result.FParent;

    Result := Result.FParent;
  end else
  begin
    Result := Result.FRight;

    while Result.FLeft <> nil do
      Result := Result.FLeft;
  end;
end;

constructor TSortedDictionary<TKey, TValue>.Create(const AArray: array of TKeyValuePair<TKey, TValue>;
  const Ascending: Boolean);
begin
  Create(TType<TKey>.Default, TType<TValue>.Default, AArray, Ascending);
end;

constructor TSortedDictionary<TKey, TValue>.Create(
  const AKeyType: IType<TKey>;
  const AValueType: IType<TValue>;
  const AArray: array of TKeyValuePair<TKey, TValue>;
  const Ascending: Boolean);
var
  I: Integer;
begin
  { Call upper constructor }
  Create(AKeyType, AValueType, Ascending);

  { Copy all items in }
  for I := 0 to Length(AArray) - 1 do
  begin
    Add(AArray[I]);
  end;
end;

constructor TSortedDictionary<TKey, TValue>.Create(const AArray: TDynamicArray<TKeyValuePair<TKey, TValue>>;
  const Ascending: Boolean);
begin
  Create(TType<TKey>.Default, TType<TValue>.Default, AArray, Ascending);
end;

constructor TSortedDictionary<TKey, TValue>.Create(const AArray: TFixedArray<TKeyValuePair<TKey, TValue>>;
  const Ascending: Boolean);
begin
  Create(TType<TKey>.Default, TType<TValue>.Default, AArray, Ascending);
end;

constructor TSortedDictionary<TKey, TValue>.Create(const AKeyType: IType<TKey>; const AValueType: IType<TValue>;
  const AArray: TDynamicArray<TKeyValuePair<TKey, TValue>>; const Ascending: Boolean);
var
  I: Cardinal;
begin
  { Call upper constructor }
  Create(AKeyType, AValueType, Ascending);

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

constructor TSortedDictionary<TKey, TValue>.Create(const AKeyType: IType<TKey>;
  const AValueType: IType<TValue>;
  const AArray: TFixedArray<TKeyValuePair<TKey, TValue>>;
  const Ascending: Boolean);
var
  I: Cardinal;
begin
  { Call upper constructor }
  Create(AKeyType, AValueType, Ascending);

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

{ TSortedDictionary<TKey, TValue>.TPairEnumerator }

constructor TSortedDictionary<TKey, TValue>.TPairEnumerator.Create(const ADict: TSortedDictionary<TKey, TValue>);
begin
  { Initialize }
  FDict := ADict;
  KeepObjectAlive(FDict);

  FNext := ADict.FindLeftMostNode();

  FVer := ADict.FVer;
end;

destructor TSortedDictionary<TKey, TValue>.TPairEnumerator.Destroy;
begin
  ReleaseObject(FDict);
  inherited;
end;

function TSortedDictionary<TKey, TValue>.TPairEnumerator.GetCurrent: TKeyValuePair<TKey,TValue>;
begin
  if FVer <> FDict.FVer then
     ExceptionHelper.Throw_CollectionChangedError();

  Result := FValue;
end;

function TSortedDictionary<TKey, TValue>.TPairEnumerator.MoveNext: Boolean;
begin
  if FVer <> FDict.FVer then
     ExceptionHelper.Throw_CollectionChangedError();

  { Do not continue on last node }
  if FNext = nil then
    Exit(false);

  { Get the current value }
  FValue := TKeyValuePair<TKey, TValue>.Create(FNext.FKey, FNext.FValue);

  { Navigate further in the tree }
  FNext := FDict.WalkToTheRight(FNext);

  Result := true;
end;

{ TSortedDictionary<TKey, TValue>.TKeyEnumerator }

constructor TSortedDictionary<TKey, TValue>.TKeyEnumerator.Create(const ADict: TSortedDictionary<TKey, TValue>);
begin
  { Initialize }
  FDict := ADict;
  KeepObjectAlive(FDict);

  FNext := ADict.FindLeftMostNode();

  FVer := ADict.FVer;
end;

destructor TSortedDictionary<TKey, TValue>.TKeyEnumerator.Destroy;
begin
  ReleaseObject(FDict);
  inherited;
end;

function TSortedDictionary<TKey, TValue>.TKeyEnumerator.GetCurrent: TKey;
begin
  if FVer <> FDict.FVer then
     ExceptionHelper.Throw_CollectionChangedError();

  Result := FValue;
end;

function TSortedDictionary<TKey, TValue>.TKeyEnumerator.MoveNext: Boolean;
begin
  if FVer <> FDict.FVer then
     ExceptionHelper.Throw_CollectionChangedError();

  { Do not continue on last node }
  if FNext = nil then
    Exit(false);

  { Get the current value }
  FValue := FNext.FKey;

  { Navigate further in the tree }
  FNext := FDict.WalkToTheRight(FNext);

  Result := true;
end;


{ TSortedDictionary<TKey, TValue>.TValueEnumerator }

constructor TSortedDictionary<TKey, TValue>.TValueEnumerator.Create(const ADict: TSortedDictionary<TKey, TValue>);
begin
  { Initialize }
  FDict := ADict;
  KeepObjectAlive(FDict);

  FNext := ADict.FindLeftMostNode();

  FVer := ADict.FVer;
end;

destructor TSortedDictionary<TKey, TValue>.TValueEnumerator.Destroy;
begin
  ReleaseObject(FDict);
  inherited;
end;

function TSortedDictionary<TKey, TValue>.TValueEnumerator.GetCurrent: TValue;
begin
  if FVer <> FDict.FVer then
     ExceptionHelper.Throw_CollectionChangedError();

  Result := FValue;
end;

function TSortedDictionary<TKey, TValue>.TValueEnumerator.MoveNext: Boolean;
begin
  if FVer <> FDict.FVer then
     ExceptionHelper.Throw_CollectionChangedError();

  { Do not continue on last node }
  if FNext = nil then
    Exit(false);

  { Get the current value }
  FValue := FNext.FValue;

  { Navigate further in the tree }
  FNext := FDict.WalkToTheRight(FNext);

  Result := true;
end;

{ TSortedDictionary<TKey, TValue>.TKeyCollection }

constructor TSortedDictionary<TKey, TValue>.TKeyCollection.Create(const ADict: TSortedDictionary<TKey, TValue>);
begin
  { Initialize }
  FDict := ADict;

  InstallType(ADict.KeyType);
end;

destructor TSortedDictionary<TKey, TValue>.TKeyCollection.Destroy;
begin
  inherited;
end;

function TSortedDictionary<TKey, TValue>.TKeyCollection.GetCount: Cardinal;
begin
  { Number of elements is the same as key }
  Result := FDict.Count;
end;

function TSortedDictionary<TKey, TValue>.TKeyCollection.GetEnumerator: IEnumerator<TKey>;
begin
  Result := TKeyEnumerator.Create(Self.FDict);
end;

procedure TSortedDictionary<TKey, TValue>.TKeyCollection.CopyTo(var AArray: array of TKey);
begin
  { Call more generic function }
  CopyTo(AArray, 0);
end;

procedure TSortedDictionary<TKey, TValue>.TKeyCollection.CopyTo(var AArray: array of TKey; const StartIndex: Cardinal);
var
  I, X: Integer;
  LNode: TNode;
begin
  { Check for indexes }
  if StartIndex >= Cardinal(Length(AArray)) then
    ExceptionHelper.Throw_ArgumentOutOfRangeError('StartIndex');

  if (Cardinal(Length(AArray)) - StartIndex) < FDict.Count then
     ExceptionHelper.Throw_ArgumentOutOfSpaceError('AArray');

  X := StartIndex;

  { Find the left-most node }
  LNode := FDict.FindLeftMostNode();

  while (LNode <> nil) do
  begin
    { Get the key }
    AArray[X] := LNode.FKey;

    { Navigate further in the tree }
    LNode := FDict.WalkToTheRight(LNode);

    { Increment the index }
    Inc(X);
  end;
end;

{ TSortedDictionary<TKey, TValue>.TValueCollection }

constructor TSortedDictionary<TKey, TValue>.TValueCollection.Create(const ADict: TSortedDictionary<TKey, TValue>);
begin
  { Initialize }
  FDict := ADict;

  InstallType(ADict.ValueType);
end;

destructor TSortedDictionary<TKey, TValue>.TValueCollection.Destroy;
begin
  inherited;
end;

function TSortedDictionary<TKey, TValue>.TValueCollection.GetCount: Cardinal;
begin
  { Number of elements is the same as key }
  Result := FDict.Count;
end;

function TSortedDictionary<TKey, TValue>.TValueCollection.GetEnumerator: IEnumerator<TValue>;
begin
  Result := TValueEnumerator.Create(Self.FDict);
end;

procedure TSortedDictionary<TKey, TValue>.TValueCollection.CopyTo(var AArray: array of TValue);
begin
  { Call more generic function }
  CopyTo(AArray, 0);
end;

procedure TSortedDictionary<TKey, TValue>.TValueCollection.CopyTo(var AArray: array of TValue; const StartIndex: Cardinal);
var
  X: Integer;
  LNode: TNode;
begin
  { Check for indexes }
  if StartIndex >= Cardinal(Length(AArray)) then
    ExceptionHelper.Throw_ArgumentOutOfRangeError('StartIndex');

  if (Cardinal(Length(AArray)) - StartIndex) < FDict.Count then
     ExceptionHelper.Throw_ArgumentOutOfSpaceError('AArray');

  X := StartIndex;

  { Find the left-most node }
  LNode := FDict.FindLeftMostNode();

  while (LNode <> nil) do
  begin
    { Get the key }
    AArray[X] := LNode.FValue;

    { Navigate further in the tree }
    LNode := FDict.WalkToTheRight(LNode);

    { Increment the index }
    Inc(X);
  end;
end;

{ TObjectSortedDictionary<TKey, TValue> }

procedure TObjectSortedDictionary<TKey, TValue>.InstallTypes(const AKeyType: IType<TKey>; const AValueType: IType<TValue>);
begin
  { Create a wrapper over the real type class and switch it }
  FKeyWrapperType := TObjectWrapperType<TKey>.Create(AKeyType);
  FValueWrapperType := TObjectWrapperType<TValue>.Create(AValueType);

  { Install overridden type }
  inherited InstallTypes(FKeyWrapperType, FValueWrapperType);
end;

function TObjectSortedDictionary<TKey, TValue>.GetOwnsKeys: Boolean;
begin
  Result := FKeyWrapperType.AllowCleanup;
end;

function TObjectSortedDictionary<TKey, TValue>.GetOwnsValues: Boolean;
begin
  Result := FValueWrapperType.AllowCleanup;
end;

procedure TObjectSortedDictionary<TKey, TValue>.SetOwnsKeys(const Value: Boolean);
begin
  FKeyWrapperType.AllowCleanup := Value;
end;

procedure TObjectSortedDictionary<TKey, TValue>.SetOwnsValues(const Value: Boolean);
begin
  FValueWrapperType.AllowCleanup := Value;
end;

end.
