unit turbu_functional;

interface
uses
   Generics.Collections;

type
   TMapper<T> = reference to procedure(const input: T);
   TMapperFunc<T, U> = reference to function(const input: T): U;
   TReducer<T> = reference to function(const input1, input2: T): T;

   TFunctional = record
   public
      class procedure sMap<T>(list: TEnumerable<T>; work: TMapper<T>); overload; static;
      class procedure sMap<T>(list: array of T; work: TMapper<T>); overload; static;
      class procedure map<T>(list: TEnumerable<T>; work: TMapper<T>); overload; static;
      class procedure map<T>(list: array of T; work: TMapper<T>); overload; static;
      class function Map<T, U>(list: TEnumerable<T>; work: TMapperFunc<T, U>): TList<U>; overload; static;
      class function Map<T, U>(list: array of T; work: TMapperFunc<T, U>): TList<U>; overload; static;
      class function reduce<T>(list: TEnumerable<T>; work: TReducer<T>): T; overload; static;
      class function reduce<T>(list: array of T; work: TReducer<T>): T; overload; static;
      class function mapReduce<T; U: class>(list: TEnumerable<T>; mapper: TMapperFunc<T, U>; reducer: TReducer<U>): U; overload; static;
      class function mapReduce<T; U: class>(list: array of T; mapper: TMapperFunc<T, U>; reducer: TReducer<U>): U; overload; static;
   end;

implementation

class procedure TFunctional.sMap<T>(list: TEnumerable<T>; work: TMapper<T>);
var
   enumerator: T;
begin
   for enumerator in list do
      work(enumerator);
end;

class procedure TFunctional.sMap<T>(list: array of T; work: TMapper<T>);
var
   i: integer;
begin
   for I := low(list) to high(list) do
      work(list[i]);
end;

class procedure TFunctional.map<T>(list: TEnumerable<T>; work: TMapper<T>);
begin
   sMap<T>(list, work); //make this parallel later
end;

class procedure TFunctional.map<T>(list: array of T; work: TMapper<T>);
begin
   sMap<T>(list, work); //make this parallel later
end;

class function TFunctional.Map<T, U>(list: TEnumerable<T>; work: TMapperFunc<T, U>): TList<U>;
var
   enumerator: T;
begin
   result := TList<U>.Create;
   for enumerator in list do
      result.Add(work(enumerator));
end;

class function TFunctional.Map<T, U>(list: array of T; work: TMapperFunc<T, U>): TList<U>;
var
   i: integer;
begin
   result := TList<U>.Create;
   for I := low(list) to high(list) do
      result.Add(work(list[i]));
end;

class function TFunctional.reduce<T>(list: TEnumerable<T>; work: TReducer<T>): T;
var
   enumerator: T;
begin
   result := default(T);
   for enumerator in list do
      result := work(result, enumerator);
end;

class function TFunctional.reduce<T>(list: array of T; work: TReducer<T>): T;
var
   i: integer;
begin
   result := default(T);
   for I := low(list) to high(list) do
      result := work(result, list[i]);
end;

class function TFunctional.mapReduce<T, U>(list: TEnumerable<T>; mapper: TMapperFunc<T, U>; reducer: TReducer<U>): U;
var
   intermediary: TList<U>;
   resultObj: U;
begin
   //work
   intermediary := Map<T, U>(list, mapper);
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

class function TFunctional.mapReduce<T, U>(list: array of T; mapper: TMapperFunc<T, U>; reducer: TReducer<U>): U;
var
   intermediary: TList<U>;
   resultObj: U;
begin
   //work
   intermediary := Map<T, U>(list, mapper);
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
