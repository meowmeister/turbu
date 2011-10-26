{*****************************************************************************
* The contents of this file are used with permission, subject to
* the Mozilla Public License Version 1.1 (the "License"); you may
* not use this file except in compliance with the License. You may
* obtain a copy of the License at
* http://www.mozilla.org/MPL/MPL-1.1.html
*
* Software distributed under the License is distributed on an
* "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
* implied. See the License for the specific language governing
* rights and limitations under the License.
*
*****************************************************************************
*
* This file was created by Mason Wheeler.  He can be reached for support at
* www.turbu-rpg.com.
*****************************************************************************}

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
    charClasses_Resists: TClientDataSet;
    charClasses_Conditions: TClientDataSet;

    condNames: TSimpleDataSet;
    attribNames: TSimpleDataSet;
    itemNames: TSimpleDataSet;
    animNames: TSimpleDataSet;
    MPartyNames: TSimpleDataSet;
    terrainNames: TSimpleDataSet;
    allVocab: TSimpleDataSet;

    shieldsid: TIntegerField;
    shieldsname: TWideStringField;
    shieldsusableByHero: TBlobField;
    weaponsTwoHanded: TBooleanField;

    srcAnimName: TDataSource;
    srcShields: TDataSource;
    srcWeapons: TDataSource;
    srcOffhands: TDataSource;
    srcMPartyNames: TDataSource;
    srcTerrainNames: TDataSource;

    procedure itemNamesAfterOpen(DataSet: TDataSet);
    procedure DataModuleCreate(Sender: TObject);
  public
    procedure EnsureItems;
    procedure EnsureAnims;
    procedure EnsureVars;
    procedure EnsureSwitches;
    procedure EnsureVocab;
    procedure EnsureMParties;
    procedure EnsureTerrain;
  end;

var
   dmDatabaseAux: TdmDatabaseAux;

implementation

{$R *.dfm}

procedure TdmDatabaseAux.DataModuleCreate(Sender: TObject);
begin
   charClasses_Resists.cloneCursor(dmDatabase.charClasses_Resists, false, true);
   charClasses_Conditions.cloneCursor(dmDatabase.charClasses_Conditions, false, true);
end;

procedure TdmDatabaseAux.EnsureAnims;
begin
   animNames.active := true;
end;

procedure TdmDatabaseAux.EnsureItems;
begin
   itemNames.active := true;
end;

procedure TdmDatabaseAux.EnsureMParties;
begin
   mpartyNames.Active := true;
end;

procedure TdmDatabaseAux.EnsureSwitches;
begin
   dmDatabase.Switches.Active := true;
end;

procedure TdmDatabaseAux.EnsureTerrain;
begin
   terrainNames.Active := true;
end;

procedure TdmDatabaseAux.EnsureVars;
begin
   dmDatabase.Variables.Active := true;
end;

procedure TdmDatabaseAux.EnsureVocab;
begin
   dmDatabase.Vocab.Active := true;
   dmDatabase.CustomVocab.Active := true;
   AllVocab.Active := true;
end;

procedure TdmDatabaseAux.itemNamesAfterOpen(DataSet: TDataSet);
var
   clone: TClientDataset;
begin
   for clone in TArray<TClientDataset>.Create(weapons, shields, armors, helmets, accessories, offhands) do
      clone.CloneCursor(itemNames, false, true);
end;

end.
