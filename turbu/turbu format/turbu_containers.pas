unit turbu_containers;

interface
uses
   SysUtils, generics.collections,
   turbu_functional;

type
   TRpgObjectList<T: class> = class(TObjectList<T>)
   private
      function getHigh: integer;
   public
      procedure map(work: TMapper<T>);
      function mapF(work: TMapperFunc<T, T>): TRpgObjectList<T>; overload;
      function mapF<U: class>(work: TMapperFunc<T, U>): TRpgObjectList<U>; overload;
      function where(filter: TPredicate<T>): TRpgObjectList<T>;
      property High: integer read getHigh;
   end;

implementation

{ TRpgObjectList<T> }

function TRpgObjectList<T>.getHigh: integer;
begin
   result := Count - 1;
end;

procedure TRpgObjectList<T>.map(work: TMapper<T>);
begin
   TFunctional.map<T>(self, work);
end;

function TRpgObjectList<T>.mapF(work: TMapperFunc<T, T>): TRpgObjectList<T>;
begin
   result := self.mapF<T>(work);
end;

function TRpgObjectList<T>.mapF<U>(work: TMapperFunc<T, U>): TRpgObjectList<U>;
var
   output: TRpgObjectList<U>;
begin
   output := TRpgObjectList<U>.Create(false);
   TFunctional.map<T>(self,
      procedure(const input: T)
      begin
         output.Add(work(input));
      end);
   result := output;
end;

function TRpgObjectList<T>.where(filter: TPredicate<T>): TRpgObjectList<T>;
var
   output: TRpgObjectList<T>;
begin
   output := TRpgObjectList<T>.Create(false);
   TFunctional.map<T>(self,
      procedure(const input: T)
      begin
         if filter(input) then
            output.Add(input);
      end);
   result := output;
end;

end.
