unit IDLookupCombo;

interface
uses
   Classes, DB, DBCtrls, DBClient;

type
   TIDLookupCombo = class(TDBLookupComboBox)
   private
      FInternalSource: TDataSource;
      FDataset: TClientDataset;
      FIDField: TIntegerField;
      function GetID: integer;
      procedure SetID(const Value: integer);
    function GetDataField: WideString;
   public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
      property id: integer read GetID write SetID;
   published
      property DataField: WideString read GetDataField stored false;
      property DataSource: TDataSource read FInternalSource stored false;
   end;

procedure Register;

implementation

procedure Register;
begin
   RegisterComponents('TURBU', [TIDLookupCombo]);
end;

{ TIDLookupCombo }

constructor TIDLookupCombo.Create(AOwner: TComponent);
const IDFIELD = 'id';
begin
   inherited Create(AOwner);
   FDataset := TClientDataset.Create(self);
   FDataset.FieldDefs.Add(IDFIELD, ftInteger);
   FDataset.CreateDataSet;
   FDataset.AppendRecord([1]);
   FDataset.First;
   FIDField := FDataset.FieldByName(IDFIELD) as TIntegerField;
   FInternalSource := TDataSource.Create(self);
   FInternalSource.DataSet := FDataset;
   inherited DataSource := FInternalSource;
   inherited DataField := IDFIELD;
end;

destructor TIDLookupCombo.Destroy;
begin
   FInternalSource.Free;
   FDataset.Free;
   inherited Destroy;
end;

function TIDLookupCombo.GetDataField: WideString;
begin
   result := inherited DataField;
end;

function TIDLookupCombo.GetID: integer;
begin
   result := FIDField.Value;
end;

procedure TIDLookupCombo.SetID(const Value: integer);
begin
   FDataset.Edit;
   FIDField.Value := value;
   FDataset.Post;
end;

end.
