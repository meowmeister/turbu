unit dataset_list_hack;

interface
uses
   DBClient, classes;

type
   TDatasetList = class;

   TDatasetListEnumerator = record
   private
      FIndex: Integer;
      FList: TDatasetList;
      function GetCurrent: TClientDataset ; inline;
   public
      constructor Create(AList: TDatasetList);
      function MoveNext: Boolean; inline;
      property Current: TClientDataset read GetCurrent;
   end;

   TDatasetList = class(TList)
   private
      function Get(Index: Integer): TClientDataset;
      procedure Put(Index: Integer; const Value: TClientDataset);
   public
      function GetEnumerator: TDatasetListEnumerator;
      property Items[Index: Integer]: TClientDataset read Get write Put; default;
   end;

implementation

{ TDatasetList }

function TDatasetList.Get(Index: Integer): TClientDataset;
begin
   result := TObject(inherited items[index]) as TClientDataset;
end;

function TDatasetList.GetEnumerator: TDatasetListEnumerator;
begin
  Result := TDatasetListEnumerator.Create(Self);
end;

procedure TDatasetList.Put(Index: Integer; const Value: TClientDataset);
begin
   inherited items[index] := value;
end;

{ TDatasetListEnumerator }

constructor TDatasetListEnumerator.Create(AList: TDatasetList);
begin
  FIndex := -1;
  FList := AList;
end;

function TDatasetListEnumerator.GetCurrent: TClientDataset;
begin
  Result := FList[FIndex];
end;

function TDatasetListEnumerator.MoveNext: Boolean;
begin
  Result := FIndex < FList.Count - 1;
  if Result then
    Inc(FIndex);
end;

end.
