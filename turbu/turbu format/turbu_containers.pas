unit turbu_containers;

interface
uses
   generics.collections;

type
   TRpgDictionary<TKey,TValue> = class(TObjectDictionary<TKey,TValue>)
   public
      destructor Destroy; override;
   end;

   TRpgList<T> = class (TList<T>)
   private
      function getHigh: integer;
      function getLength: integer;
   public
      function Last: T;
      property High: integer read getHigh;
      property Length: integer read getLength;
   end;

   TRpgObjectList<T: class> = class(TObjectList<T>)
   private
      function getHigh: integer;
      function getLength: integer;
   public
      function Last: T;
      property High: integer read getHigh;
      property Length: integer read getLength;
   end;

implementation

{ TRpgDictionary<TKey, TValue> }

destructor TRpgDictionary<TKey, TValue>.Destroy;
begin
   self.Keys.Free;
   self.Values.Free;
   inherited Destroy;
end;

{ TRpgList<T> }

function TRpgList<T>.GetHigh: integer;
begin
   result := self.Count - 1;
end;

function TRpgList<T>.getLength: integer;
begin
   result := self.High + 1;
end;

function TRpgList<T>.Last: T;
begin
   result := Self[High];
end;

{ TRpgObjectList<T> }

function TRpgObjectList<T>.getHigh: integer;
begin
   result := Count - 1;
end;

function TRpgObjectList<T>.getLength: integer;
begin
   result := self.High + 1;
end;

function TRpgObjectList<T>.Last: T;
begin
   result := Self[High];
end;

end.
