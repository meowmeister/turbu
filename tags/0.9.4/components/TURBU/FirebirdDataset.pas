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
* tech.turbu-rpg.com.
*****************************************************************************}
unit FirebirdDataset;

interface
uses
   SimpleDS, DB;

type
  TSimpleDataset = class(SimpleDS.TSimpleDataset)
  protected
    procedure AllocDataSet; override;
    procedure DataEvent(Event: TDataEvent; Info: Longint); override;
  end;

implementation
uses
  Classes, SysUtils, RTTI, Provider, SqlExpr,
  DBCommon;

type
  TInternalSQLDataSet = class(SimpleDS.TInternalSQLDataSet)
  private
    FQuery: TSqlQuery;
  protected
    procedure InternalInitFieldDefs; override;
  public
    constructor Create(AOwner: TComponent); overload; override;
  end;

{ TSimpleDataset }

procedure TSimpleDataset.AllocDataSet;
var
  ctx: TRttiContext;
  cls: TRttiType;
  fld: TRttiField;
  FDataset: TInternalSQLDataSet;
  FProvider: TDataSetProvider;
begin
  cls := ctx.GetType(self.ClassType);

  fld := cls.GetField('FDataset');
  FDataSet := TInternalSQLDataSet.Create(Self);
  FDataSet.Name := 'InternalDataSet';
  FDataSet.SQLConnection := Connection;
  FDataSet.SetSubComponent(True);
  fld.SetValue(self, FDataset);

  fld := cls.GetField('FProvider');
  FProvider := fld.GetValue(self).AsType<TDataSetProvider>;
  FProvider.DataSet := FDataSet;
end;

procedure TSimpleDataset.DataEvent(Event: TDataEvent; Info: Integer);
begin
  if not (ControlsDisabled and (event = deUpdateState)) then
    inherited DataEvent(event, info);
end;

{function TSimpleDataset.GetProvider: TDatasetProvider;
begin
   result := TRttiContext.Create.GetType(self.ClassType).GetField('FProvider').GetValue(self).AsObject as TDatasetProvider;
end;}

{ TInternalSQLDataSet }

constructor TInternalSQLDataSet.Create(AOwner: TComponent);
const
  QUERY = 'select rFields.RDB$FIELD_NAME NAME ' +
          'from  RDB$RELATION_FIELDS rFields ' +
          'join RDB$FIELDS fields ON (rFields.RDB$FIELD_SOURCE = fields.RDB$FIELD_NAME) ' +
          'where  (rFields.RDB$RELATION_NAME=:tablename) and (fields.RDB$FIELD_TYPE = 7) ' +
          '  and (rFields.RDB$FIELD_SOURCE = ''BOOLEAN'') ' +
          'order by rFields.RDB$FIELD_POSITION ';
begin
  inherited Create(AOwner);
  FQuery := TSqlQuery.Create(self);
  FQuery.SQL.Text := QUERY;
  FQuery.Params.AddParameter.Name := 'tablename';
end;

procedure TInternalSQLDataSet.InternalInitFieldDefs;
var
  def: TFieldDef;
begin
  inherited InternalInitFieldDefs;
  if not (self.CommandType in [ctTable, ctQuery]) then
    Exit;

  FQuery.Active := false;
  //Why is TCustomSQLDataSet.SetConnection both virtual and private?!?
  FQuery.SQLConnection := self.SQLConnection;
  FQuery.ParamByName('tablename').AsString := UpperCase(GetTableNameFromQuery(self.CommandText));
  FQuery.Active := true;
  FQuery.First;
  FieldDefs.BeginUpdate;
  try
    while not FQuery.Eof do
    begin
      def := TFieldDef(TDefCollection(FieldDefs).Find(Trim(FQuery.FieldByName('NAME').AsString)));
      if assigned(def) then
         def.DataType := ftBoolean;
      FQuery.Next;
    end;
  finally
    FieldDefs.EndUpdate;
  end;
end;

end.
