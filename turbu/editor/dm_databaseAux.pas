unit dm_databaseAux;

interface

uses
  SysUtils, Classes, SimpleDS, DB, DBClient,
  dm_database, FirebirdDataset;

type
  TdmDatabaseAux = class(TDataModule)
    shields: TClientDataSet;
    armors: TClientDataSet;
    helmets: TClientDataSet;
    accessories: TClientDataSet;
    weapons: TClientDataSet;
    offhands: TClientDataSet;
    condNames: TSimpleDataSet;
    attribNames: TSimpleDataSet;
    itemNames: TSimpleDataSet;
    animNames: TSimpleDataSet;

    shieldsid: TIntegerField;
    shieldsname: TWideStringField;
    shieldsusableByHero: TBlobField;
    weaponsTwoHanded: TBooleanField;

    srcAnimName: TDataSource;
    srcShields: TDataSource;
    srcWeapons: TDataSource;
    srcOffhands: TDataSource;

    procedure itemNamesAfterOpen(DataSet: TDataSet);
  public
    procedure EnsureItems;
    procedure EnsureAnims;
    procedure EnsureVars;
    procedure EnsureSwitches;
    procedure EnsureVocab;
  end;

var
   dmDatabaseAux: TdmDatabaseAux;

implementation

{$R *.dfm}

procedure TdmDatabaseAux.EnsureAnims;
begin
   animNames.active := true;
end;

procedure TdmDatabaseAux.EnsureItems;
begin
   itemNames.active := true;
end;

procedure TdmDatabaseAux.EnsureSwitches;
begin
   dmDatabase.Switches.Active := true;
end;

procedure TdmDatabaseAux.EnsureVars;
begin
   dmDatabase.Variables.Active := true;
end;

procedure TdmDatabaseAux.EnsureVocab;
begin
   dmDatabase.Vocab.Active := true;
   dmDatabase.CustomVocab.Active := true;
end;

procedure TdmDatabaseAux.itemNamesAfterOpen(DataSet: TDataSet);
var
   clone: TClientDataset;
begin
   for clone in TArray<TClientDataset>.Create(weapons, shields, armors, helmets, accessories, offhands) do
      clone.CloneCursor(itemNames, false, true);
end;

end.
