unit turbu_functional;

interface
uses
   Generics.Collections, SysUtils;

type
   TMapper<T> = reference to procedure(const input: T);
   TMapperFunc<T, U> = reference to function(const input: T): U;
   TReducer<T> = reference to function(const input1, input2: T): T;
   TReducer<T, U> = reference to function(const input1: T; input2: U): U;

   TFunctional = class
   public
      class procedure sMap<T>(list: TEnumerable<T>; work: TMapper<T>); static;
      class procedure map<T>(list: TEnumerable<T>; work: TMapper<T>); static;
      class function mapF<T, U>(list: TEnumerable<T>; work: TMapperFunc<T, U>): TList<U>; static;
      class function reduce<T>(list: TEnumerable<T>; work: TReducer<T>): T; static;
      class function reduce2<T, U>(list: TEnumerable<T>; work: TReducer<T, U>): U; static;
      class function mapReduce<T; U: class>(list: TEnumerable<T>; mapper: TMapperFunc<T, U>; reducer: TReducer<U>): U; static;
      class function zip<T, U>(list1: TEnumerable<T>; list2: TEnumerable<U>): TList<TPair<T, U>>; static;
      class function countWhere<T>(list: TEnumerable<T>; countFunc: TPredicate<T>): integer; static;
      class function count<T>(list: TEnumerable<T>): integer; static;
      class function where<T>(list: TEnumerable<T>; filter: TPredicate<T>): TList<T>;
      class function firstWhere<T>(list: TEnumerable<T>; filter: TPredicate<T>): T;
   end;

implementation

class procedure TFunctional.sMap<T>(list: TEnumerable<T>; work: TMapper<T>);
var
   enumerator: T;
begin
   for enumerator in list do
      work(enumerator);
end;

class function TFunctional.where<T>(list: TEnumerable<T>; filter: TPredicate<T>): TList<T>;
var
   enumerator: T;
begin
   result := TList<T>.Create;
   for enumerator in list do
      if filter(enumerator) then
         result.Add(enumerator);
end;

class function TFunctional.firstWhere<T>(list: TEnumerable<T>;
  filter: TPredicate<T>): T;
var
   enumerator: T;
begin
   for enumerator in list do
      if filter(enumerator) then
         exit(enumerator);
end;

class function TFunctional.zip<T, U>(list1: TEnumerable<T>; list2: TEnumerable<U>): TList<TPair<T, U>>;
var
   enum1: TEnumerator<T>;
   enum2: TEnumerator<U>;
   firstContinue: boolean;
begin
   if not (assigned(list1) and assigned(list2)) then
      raise EArgumentException.Create('Nil list passed to Zip function');
   enum1 := nil;
   enum2 := nil;
   enum1 := list1.GetEnumerator;
   enum2 := list2.GetEnumerator;
   result := TList<TPair<T, U>>.Create;
   while (enum1.MoveNext) and (enum2.MoveNext) do
      result.Add(TPair<T,U>.Create(enum1.Current, enum2.Current));
   enum1.Free;
   enum2.Free;
end;

class function TFunctional.count<T>(list: TEnumerable<T>): integer;
begin
   result := countWhere<T>(list,
      function(Arg1: T): Boolean
      begin
         result := true;
      end);
end;

class function TFunctional.countWhere<T>(list: TEnumerable<T>; countFunc: TPredicate<T>): integer;
var
   count: integer;
begin
   count := 0;
   map<T>(list,
      procedure (const input: T)
      begin
         if countFunc(input) then
            inc(count);
      end);
   result := count;
end;

class procedure TFunctional.map<T>(list: TEnumerable<T>; work: TMapper<T>);
begin
   sMap<T>(list, work); //make this parallel later
end;

class function TFunctional.MapF<T, U>(list: TEnumerable<T>; work: TMapperFunc<T, U>): TList<U>;
var
   enumerator: T;
begin
   result := TList<U>.Create;
   for enumerator in list do
      result.Add(work(enumerator));
end;

class function TFunctional.reduce<T>(list: TEnumerable<T>; work: TReducer<T>): T;
var
   enumerator: T;
begin
   result := default(T);
   for enumerator in list do
      result := work(result, enumerator);
end;

class function TFunctional.reduce2<T, U>(list: TEnumerable<T>; work: TReducer<T, U>): U;
var
   enumerator: T;
begin
   result := default(U);
   for enumerator in list do
      result := work(enumerator, result);
end;

class function TFunctional.mapReduce<T, U>(list: TEnumerable<T>; mapper: TMapperFunc<T, U>; reducer: TReducer<U>): U;
var
   intermediary: TList<U>;
   resultObj: U;
begin
   //work
   intermediary := MapF<T, U>(list, mapper);
   resultObj := reduce<U>(intermediary, reducer);

   //cleanup
   map<U>(intermediary,
      procedure(const input: U)
      begin
         if input <> resultObj then
            input.Free;
      end);
   intermediary.Free;
   result := resultObj;
end;

end.
