unit decl_list_hack;

interface
uses
   classes,
   turbu_defs;

type
   TDeclHackList = class;

   TDeclListEnumerator = record
   private
      FIndex: Integer;
      FList: TDeclHackList;
      function GetCurrent: TNameType; inline;
   public
      constructor Create(AList: TDeclHackList);
      function MoveNext: Boolean; inline;
      property Current: TNameType read GetCurrent;
   end;

   TDeclHackList = class(TList)
   private
      function Get(Index: Integer): TNameType;
      procedure Put(Index: Integer; const Value: TNameType);
   public
      function high: integer; inline;
      function Add(Item: TNameType): Integer;
      function GetEnumerator: TDeclListEnumerator;
      property Items[Index: Integer]: TNameType read Get write Put; default;
   end;

implementation

type
   PNameType = ^TNameType;

{ TDeclHackList }

function TDeclHackList.Add(Item: TNameType): Integer;
var
   ptr: PNameType;
begin
   new(ptr);
   ptr^ := item;
   result := inherited Add(ptr);
end;

function TDeclHackList.Get(Index: Integer): TNameType;
begin
   result := PNameType(inherited items[index])^;
end;

function TDeclHackList.GetEnumerator: TDeclListEnumerator;
begin
  Result := TDeclListEnumerator.Create(Self);
end;

function TDeclHackList.high: integer;
begin
   result := count - 1;
end;

procedure TDeclHackList.Put(Index: Integer; const Value: TNameType);
var
   ptr: PNameType;
begin
   new(ptr);
   ptr^ := value;
   inherited items[index] := ptr;
end;

{ TDeclListEnumerator }

constructor TDeclListEnumerator.Create(AList: TDeclHackList);
begin
  FIndex := -1;
  FList := AList;
end;

function TDeclListEnumerator.GetCurrent: TNameType;
begin
  Result := FList[FIndex];
end;

function TDeclListEnumerator.MoveNext: Boolean;
begin
  Result := FIndex < FList.Count - 1;
  if Result then
    Inc(FIndex);
end;

end.
