unit turbu_multimaps;

interface
uses
   Generics.Defaults, Generics.Collections;

type
   TMultimap<TKey, TValue> = class(TObjectDictionary<TKey, TList<TValue>>)
   private
      FLastKey: TKey;
      FLast: TList<TValue>;
      procedure GetComparer;
   protected
      FKeyComparer: IEqualityComparer<TKey>;
      procedure EnsureComparer;
      procedure EnsureList(const key: TKey);
   public
      constructor Create; overload;
      procedure Add(const key: TKey; const value: TValue); virtual;
      procedure RemovePair(const key: TKey; const value: TValue);
      procedure Clear;
      function KeyHasValue(const key: TKey; const value: TValue): boolean;
   end;

   TDistinctMultimap<TKey, TValue> = class(TMultimap<TKey, TValue>)
   public
      procedure Add(const key: TKey; const value: TValue); override;
   end;

implementation
uses
   RTTI;

{ TMultimap<TKey, TValue> }

procedure TMultimap<TKey, TValue>.Add(const key: TKey; const value: TValue);
begin
   EnsureComparer;
   EnsureList(key);
   FLast.Add(value);
end;

procedure TMultimap<TKey, TValue>.Clear;
begin
   FLastKey := Default(TKey);
   FLast := nil;
   inherited Clear;
end;

constructor TMultimap<TKey, TValue>.Create;
begin
   inherited Create([doOwnsValues]);
end;

procedure TMultimap<TKey, TValue>.EnsureComparer;
begin
   if FKeyComparer = nil then
      GetComparer;
end;

procedure TMultimap<TKey, TValue>.EnsureList(const key: TKey);
begin
   if not (assigned(FLast) and FKeyComparer.Equals(key, FLastKey)) then
   begin
      if not self.TryGetValue(key, FLast) then
      begin
         FLast := TList<TValue>.Create;
         inherited Add(key, FLast);
      end;
   end;
end;

procedure TMultimap<TKey, TValue>.GetComparer;
begin
   FKeyComparer := TRttiContext.Create.GetType(self.ClassParent.ClassInfo).
     GetField('FComparer').GetValue(self).AsType<IEqualityComparer<TKey>>;
end;

function TMultimap<TKey, TValue>.KeyHasValue(const key: TKey;
  const value: TValue): boolean;
begin
   result := self.ContainsKey(key) and self[key].Contains(value);
end;

procedure TMultimap<TKey, TValue>.RemovePair(const key: TKey;
  const value: TValue);
begin
   EnsureComparer;
   self[key].Remove(value);
end;

{ TDistinctMultimap<TKey, TValue> }

procedure TDistinctMultimap<TKey, TValue>.Add(const key: TKey;
  const value: TValue);
var
   index: integer;
begin
   EnsureComparer;
   EnsureList(key);
   if not FLast.BinarySearch(value, index) then
      FLast.Insert(index, value);
end;

end.
