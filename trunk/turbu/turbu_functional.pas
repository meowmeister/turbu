unit turbu_functional;

interface
uses
   Generics.Collections;

type
   TMapper<T> = reference to procedure(const input: T);
   TReducer<T> = reference to function(const input1, input2: T): T;

   TFunctional = record
   public
      class procedure sMap<T>(list: TEnumerable<T>; work: TMapper<T>); overload; static;
      class procedure sMap<T>(list: array of T; work: TMapper<T>); overload; static;
      class procedure map<T>(list: TEnumerable<T>; work: TMapper<T>); overload; static;
      class procedure map<T>(list: array of T; work: TMapper<T>); overload; static;
      class function reduce<T>(list: TEnumerable<T>; work: TReducer<T>): T; overload; static;
      class function reduce<T>(list: array of T; work: TReducer<T>): T; overload; static;
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

end.
