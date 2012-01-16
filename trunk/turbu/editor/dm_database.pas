unit dm_database;
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

interface

uses
   SysUtils, Classes, DBClient, DB, Generics.Collections, RTTI,
   SimpleDS, SqlExpr, FMTBcd,
   turbu_database_interface, turbu_classes, FirebirdDataset;

type
   TDatasetList = class(TList<TCustomClientDataSet>)
   public
      function FindByName(const name: string): TCustomClientDataset;
   end;

   TRelationAttribute = class(TCustomAttribute);
   ExclusionAttribute = class(TCustomAttribute);
   VitalDatasetAttribute = class(TCustomAttribute);

   TdmDatabase = class(TDataModule, IRpgDatastore)
      [VitalDataset]
      charClasses: TSimpleDataSet;
      [TRelation]
      [VitalDataset]
      charClasses_skillset: TSimpleDataSet;
      [TRelation]
      [VitalDataset]
      charClasses_Resists: TSimpleDataSet;
      [TRelation]
      [VitalDataset]
      charClasses_Conditions: TSimpleDataSet;
      animations: TSimpleDataSet;
      items: TSimpleDataSet;
      items_script: TClientDataSet;
      items_armor: TClientDataSet;
      items_weapon: TClientDataSet;
      items_medicine: TClientDataSet;
      items_book: TClientDataSet;
      items_skill: TClientDataSet;
      items_upgrade: TClientDataSet;
      items_variable: TClientDataSet;
      [TRelation]
      items_attributes: TSimpleDataSet;
      items_junk: TClientDataSet;
      [VitalDataset]
      commands: TSimpleDataSet;
      skills: TSimpleDataSet;
      [TRelation]
      skills_attributes: TSimpleDataSet;
      attributes: TSimpleDataSet;
      conditions: TSimpleDataSet;
      scriptRange: TClientDataSet;
      [TRelation]
      animations_timingSec: TSimpleDataSet;
      [TRelation]
      animations_frameSec: TSimpleDataSet;
      [VitalDataset]
      tilesets: TSimpleDataSet;
      [TRelation]
      [VitalDataset]
      tilesets_records: TSimpleDataSet;
//      [VitalDataset]
      tilegroups: TSimpleDataSet;
      [VitalDataset]
      heroes: TSimpleDataSet;
      [TRelation]
      [VitalDataset]
      heroes_Conditions: TSimpleDataSet;
      [TRelation]
      [VitalDataset]
      heroes_Resists: TSimpleDataSet;
      [TRelation]
      [VitalDataset]
      heroes_skillset: TSimpleDataSet;
      [VitalDataset]
      Floats: TSimpleDataSet;
      [VitalDataset]
      Strings: TSimpleDataSet;
      [VitalDataset]
      Switches: TSimpleDataSet;
      [VitalDataset]
      Variables: TSimpleDataSet;
      [VitalDataset]
      GlobalScripts: TSimpleDataSet;
      Vocab: TSimpleDataSet;
      CustomVocab: TSimpleDataSet;
      [VitalDataset]
      vehicles: TSimpleDataSet;
      [TRelation]
      items_AnimData: TSimpleDataSet;
      LegacyData: TSimpleDataSet;
      [VitalDataset]
      syslayout: TSimpleDataSet;
      [VitalDataset]
      MapTree: TSimpleDataSet;
      [VitalDataset]
      StartLocs: TSimpleDataSet;
      [VitalDataset]
      metadata: TSimpleDataSet;
      [VitalDataset]
      [TRelation]
      metadata_regions: TSimpleDataSet;
      [VitalDataset]
      dbData: TSimpleDataSet;
      [VitalDataset]
      boot: TSimpleDataSet;
      monsters: TSimpleDataSet;
      [TRelation]
      monsters_Conditions: TSimpleDataSet;
      [TRelation]
      monsters_Resists: TSimpleDataSet;
      mparties: TSimpleDataSet;
      [TRelation]
      mparties_monsters: TSimpleDataSet;
      [TRelation]
      mparties_events: TSimpleDataSet;
      battleChars: TSimpleDataSet;
      [TRelation]
      battleChars_Poses: TSimpleDataSet;
      [TRelation]
      battleChars_Weapons: TSimpleDataSet;
      terrain: TSimpleDataSet;
      [VitalDataset]
      SysSound: TSimpleDataSet;
      [Exclusion]
      MapObjects: TClientDataSet;
      script_cache: TSimpleDataSet;

      dsCharClasses: TDataSource;
      ArbitraryQuery: TSQLQuery;
      Connection: TSQLConnection;

      charClassesstaticEq: TBooleanField;
      itemsConditions: TBlobField;
      charClassesid: TIntegerField;
      itemsscript: TWideMemoField;
      heroesportraitShiftFColorSet1: TFloatField;
      charClassesName: TWideStringField;
      animations_timingSecframe: TWordField;
      charClassescommands: TByteField;
      skillsmagnitude: TSmallintField;
      heroesportraitShiftFHue: TShortintField;

      procedure DataModuleCreate(Sender: TObject);
      procedure restoreClone(DataSet: TDataSet);
      procedure charClasses_skillsetCalcFields(DataSet: TDataSet);
      procedure DataModuleDestroy(Sender: TObject);
      procedure classFilter(DataSet: TDataSet; var Accept: Boolean);
      procedure SwitchesVarsCalcFields(DataSet: TDataSet);
      procedure itemsAfterOpen(DataSet: TDataSet);
      procedure script_cacheReconcileError(DataSet: TCustomClientDataSet;
        E: EReconcileError; UpdateKind: TUpdateKind;
        var Action: TReconcileAction);
   private
      { Private declarations }

      FAllDatasetList: TDatasetList;
      FDatasetList: TDatasetList;
      FVitalList: TDatasetList;
      FDBName: string;
      function usableByFilter(field: TBlobField; master: TDataset): boolean;
      function GetDBScript: string;
      function FieldType(field: TField): string;
      function IndexGen(dset: TSimpleDataset): string;
      function MasterIndexGen(dset: TSimpleDataset): string;
      function ScriptGen(dset: TSimpleDataset): string;
      procedure OpenConnection(const dbname: string);
      function GetTableCount: integer;
   public
      { Public declarations }
      procedure beginUpload;
      procedure endUpload;
      function NameLookup(const name: string; id: integer): string;
      function ScriptLookup(id: integer): string;
      procedure BuildDatabase(const dbname: string; dbObj: TRpgDatafile);
      procedure Connect(const dbname: string; const validateProc: TProc<TSqlQuery>);
      procedure SaveAll(report: TUploadReportProc = nil);
      procedure EnsureTileGroups;
      function OnClosedDataset(ds: TDataset): TDataset;
      procedure OnReleaseClosedDataset(ds: TDataset);

      property TableCount: integer read GetTableCount;
      property datasets: TDatasetList read FDatasetList write FDatasetList;
      property dbScript: string read GetDBScript;
      property dbName: string read FDBName;
   end;

var
   dmDatabase: TdmDatabase;

implementation

uses
   Variants, Generics.Defaults, Windows, ZLib, DSIntf, DBXDynalink, DBXCommon,
   commons,
   turbu_skills, turbu_defs, rttiHelper, EventBuilder;

{$R *.dfm}

function CDSComparer(const Left, Right: TCustomClientDataset): Integer;
begin
   result := StrIComp(PChar(left.name), PChar(right.name));
end;

procedure TdmDatabase.DataModuleCreate(Sender: TObject);
var
   dataset: TCustomClientDataset;
   context: TRttiContext;
   instance: TRttiInstanceType;
   field: TRttiField;
begin
   context := TRttiContext.Create;
   instance := context.GetType(TdmDatabase) as TRttiInstanceType;

   FAllDatasetList := TDatasetList.Create;
   FDatasetList := TDatasetList.Create(TComparer<TCustomClientDataset>.Construct(CDSComparer));
   FVitalList := TDatasetList.Create;
   for field in instance.GetDeclaredFields do
   begin
      if not (field.FieldType is TRttiInstanceType) then
         Continue;
      if TRttiInstanceType(field.FieldType).metaclassType.InheritsFrom(TCustomClientDataset) then
      begin
         dataset := field.GetValue(self).AsObject as TCustomClientDataset;
         if not assigned(field.GetAttribute(TRelationAttribute)) then
            FDatasetList.Add(dataset);
         if not assigned(field.GetAttribute(ExclusionAttribute)) then
            FAllDatasetList.Add(dataset);
         if assigned(field.GetAttribute(VitalDatasetAttribute)) then
            FVitalList.Add(dataset);
      end;
   end;
   FDatasetList.Sort;
end;

procedure TdmDatabase.SaveAll(report: TUploadReportProc = nil);
var
   dset: TCustomClientDataset;
begin
   for dset in FAllDatasetList do
      if (dset is TSimpleDataSet) and (dset.Active) then
      begin
         if assigned(report) then
            report(format('Saving data: %s', [dset.Name]));
         dset.ApplyUpdates(0);
         if not FVitalList.Contains(dset) then
            dset.Close;
      end;
end;

procedure TdmDatabase.Connect(const dbname: string; const validateProc: TProc<TSqlQuery>);
var
   dset: TCustomClientDataset;
begin
   if not Connection.Connected then
      OpenConnection(dbname);
   if assigned(validateProc) then
      validateProc(ArbitraryQuery);

   for dset in FAllDatasetList do
      if dset is TSimpleDataset then
         TSimpleDataset(dset).DataSet.CommandText := UpperCase(TSimpleDataset(dset).DataSet.CommandText);
   for dset in FVitalList do
      try
         dset.Active := true;
      except
         OutputFormattedString('Unable to open table %s.', [dset.Name]);
      end;
   TThread.CreateAnonymousThread(
      procedure
      begin
         TMonitor.Enter(tilegroups);
         try
            tilegroups.Active := true;
         finally
            TMonitor.Exit(tilegroups);
         end;
      end).Start;
end;

procedure TdmDatabase.DataModuleDestroy(Sender: TObject);
begin
   TEBObject.Datastore := nil;
   FVitalList.Free;
   FDatasetList.Free;
   FAllDatasetList.Free;
end;

procedure TdmDatabase.beginUpload;
var
   ds: TCustomClientDataset;
begin
   for ds in FAllDatasetList do
   begin
      ds.AutoCalcFields := false;
      ds.DisableControls;
   end;
end;

procedure TdmDatabase.endUpload;
var
   ds: TCustomClientDataset;
begin
   for ds in FAllDatasetList do
   begin
      ds.AutoCalcFields := true;
      ds.EnableControls;
   end;
end;

procedure TdmDatabase.EnsureTileGroups;
begin
   TMonitor.Enter(tilegroups, INFINITE);
   TMonitor.Exit(tilegroups);
end;

function TdmDatabase.FieldType(field: TField): string;
begin
   case field.DataType of
      ftSmallint, ftWord, ftShortint, ftByte: result := 'SMALLINT';
      ftInteger, ftLongWord: result := 'INTEGER';
      ftBoolean: result := 'BOOLEAN';
      ftCurrency: result := 'DECIMAL(18, 4)';
      ftBytes, ftVarBytes, ftBlob: result := 'BLOB';
      ftWideMemo: result := 'BLOB SUB_TYPE TEXT';
      ftWideString: result := format('VARCHAR(%d) CHARACTER SET UTF8', [field.Size]);
      ftLargeint: result := 'BIGINT';
      ftSingle, ftFloat, ftExtended: result := 'REAL';
      else assert(false);
   end;
end;

function TdmDatabase.MasterIndexGen(dset: TSimpleDataset): string;
const
   FK = 'ALTER TABLE $SUB' + CRLF + '  ADD CONSTRAINT FK_$SUB' + CRLF +
        '  FOREIGN KEY (MASTER)' + CRLF + '    REFERENCES $MASTER(ID);' + CRLF;
   IX_MX = 'CREATE UNIQUE INDEX IX_$SUB' + CRLF + '  ON $SUB' + CRLF + '  (MASTER, X);';
   IX_MID = 'ALTER TABLE $SUB' + CRLF + '  ADD CONSTRAINT PK_$SUB' + CRLF + '  PRIMARY KEY (MASTER, ID);';
var
   masterIdx: integer;
   master: string;
begin
   result := FK;
   if dset.FindField('X') <> nil then
      result := result + IX_MX
   else if dset.FindField('skill') <> nil then
      result := result + StringReplace(IX_MID, 'ID', 'SKILL', [])
   else begin
      dset.FieldByName('ID');
      result := result + IX_MID;
   end;
   result := StringReplace(result, '$SUB', dset.Name, [rfReplaceAll]);
   masterIdx := Pos('_', dset.Name);
   assert(masterIdx > 0);
   master := Copy(dset.Name, 1, masterIdx - 1);
   result := StringReplace(result, '$MASTER', master, []);
end;

function TdmDatabase.IndexGen(dset: TSimpleDataset): string;
const
   INDEX = 'ALTER TABLE %s' + CRLF + '  ADD CONSTRAINT PK_%s' + CRLF + '  PRIMARY KEY (ID);';
   PK_MUS = 'ALTER TABLE %s' + CRLF + '  ADD CONSTRAINT PK_%s' + CRLF + '  PRIMARY KEY (ID, ISMUSIC);';
   PK_LEG = 'ALTER TABLE %s' + CRLF + '  ADD CONSTRAINT PK_%s' + CRLF + '  PRIMARY KEY (NAME, ID, SECTION);';
   PK_TG = 'ALTER TABLE %s' + CRLF + '  ADD CONSTRAINT PK_%s' + CRLF + '  PRIMARY KEY (NAME);';
begin
   if assigned(dset.FindField('MASTER')) then
      result := MasterIndexGen(dset)
   else if dset = LegacyData then
      result := StringReplace(PK_LEG, '%s', dset.Name, [rfReplaceAll])
   else if dset = tilegroups then
      result := StringReplace(PK_TG, '%s', dset.Name, [rfReplaceAll])
   else if dset = SysSound then
      result := StringReplace(PK_MUS, '%s', dset.Name, [rfReplaceAll])
   else if assigned(dset.FindField('ID')) then
      result := StringReplace(INDEX, '%s', dset.Name, [rfReplaceAll])
   else result := '';
   if result <> '' then
      result := CRLF + result;
end;

procedure TdmDatabase.itemsAfterOpen(DataSet: TDataSet);
var
   clone: TCustomClientDataset;
begin
   for clone in FDatasetList do
   begin
      if Pos('items_', clone.Name) = 1 then
         clone.CloneCursor(items, false);
      clone.tag := nativeInt(clone.CloneSource);
   end;
end;

function TdmDatabase.ScriptGen(dset: TSimpleDataset): string;
const
   CREATE_SCRIPT = 'CREATE TABLE %s (';
   CREATE_FIELD = '   %s %s%s,';
   NULLS: array[boolean] of string = ('', ' NOT NULL');
var
   field: TField;
begin
   result := format(CREATE_SCRIPT, [dset.Name]);
   for field in dset.fields do
      if field.FieldKind = fkData then
         result := result + CRLF + format(CREATE_FIELD, [field.FieldName, FieldType(field), NULLS[field.Required]]);
   delete(result, length(result), 1);
   result := result + ');' + IndexGen(dset);
end;

function TdmDatabase.GetDBScript: string;
const
   VERSION = 'CREATE TABLE DB_VERSION (' + CRLF + ' ID INTEGER NOT NULL, ' + CRLF + '  NAME VARCHAR(255) NOT NULL' + CRLF + ');';
   ALLVOCAB = 'CREATE VIEW ALLVOCAB ("KEY", VAL) AS ' + CRLF + 'SELECT "KEY", VAL FROM CUSTOMVOCAB ' + CRLF + 'union all ' + CRLF + 'SELECT "KEY", VAL FROM VOCAB;';
var
   component: TComponent;
   output: TStringList;
begin
   output := TStringList.Create;
   try
      output.Add(VERSION);
      for component in dmDatabase do
      begin
         if not (component is TSimpleDataset) then
            continue;
         output.Add(ScriptGen(TSimpleDataset(component)));
      end;
      output.Add(ALLVOCAB);
      result := output.text;
   finally
      output.Free;
   end;
end;

function TdmDatabase.GetTableCount: integer;
var
   dset: TDataset;
begin
   result := 0;
   for dset in FAllDatasetList do
      if dset is TSimpleDataSet then
         inc(result);
end;

function TdmDatabase.NameLookup(const name: string; id: integer): string;
var
   dataset: TCustomClientDataset;
   lResult: variant;
begin
   dataset := FDatasetList.FindByName(name);
   if not assigned(dataset) then
      exit(BAD_LOOKUP);

   if dataset.Active then
   begin
      lResult := dataset.Lookup('id', id, 'name');
      if lResult = Null then
         result := BAD_LOOKUP
      else result := lResult;
   end
   else begin
      ArbitraryQuery.Active := false;
      ArbitraryQuery.SQL.Text := format('select name from %s where id = %d', [UpperCase(name), id]);
      arbitraryQuery.Open;
      if ArbitraryQuery.RecordCount = 0 then
         result := BAD_LOOKUP
      else result := ArbitraryQuery.FieldByName('name').AsString;
   end;
end;

function TdmDatabase.ScriptLookup(id: integer): string;
var
   lResult: variant;
begin
   if script_cache.Active then
   begin
      lResult := script_cache.Lookup('id', id, 'script');
      if lResult = Null then
         result := BAD_LOOKUP
      else result := lResult;
   end
   else begin
      ArbitraryQuery.Active := false;
      ArbitraryQuery.SQL.Text := format('select SCRIPT from SCRIPT_CACHE where ID = %d', [id]);
      arbitraryQuery.Open;
      if ArbitraryQuery.RecordCount = 0 then
         result := BAD_LOOKUP
      else result := ArbitraryQuery.FieldByName('script').AsString;
   end;
end;

procedure TdmDatabase.script_cacheReconcileError(DataSet: TCustomClientDataSet;
  E: EReconcileError; UpdateKind: TUpdateKind; var Action: TReconcileAction);
begin
   action := raCancel;
end;

var
  LSetSmallint: TDBXWritableRow_SetInt16;

function DBXWritableRow_SetBoolean(Handle: TDBXWritableRowHandle;
  Ordinal: TInt32; Value: LongBool): TDBXErrorCode; stdcall;
begin
  if value then
     result := LSetSmallint(handle, ordinal, 1)
  else result := LSetSmallint(handle, ordinal, 0);
end;

function TdmDatabase.OnClosedDataset(ds: TDataset): TDataset;
const QUERY = 'select * from %s where %s';
var
   table: string;
   sd: TSimpleDataset;
begin
   sd := ds as TSimpleDataset;
   table := sd.DataSet.CommandText;
   sd.DataSet.CommandType := ctQuery;
   sd.DataSet.CommandText := format(QUERY, [table, ds.Filter]);
   ds.Filtered := false;
   ds.Filter := table;
   ds.Active := true;
   result := ds;
end;

procedure TdmDatabase.OnReleaseClosedDataset(ds: TDataset);
var
   sd: TSimpleDataset;
begin
   ds.Active := false;
   sd := ds as TSimpleDataset;
   sd.DataSet.CommandType := ctTable;
   sd.DataSet.CommandText := ds.Filter;
end;

procedure TdmDatabase.OpenConnection(const dbname: string);
var
   ctx: TRttiContext;
   cls: TRttitype;
   fld: TRttiField;
   driver: TDBXDriver;
   table: TDBXMethodTable;
begin
   connection.DriverName := 'Firebird';
   connection.Params.Values['User_Name'] := 'SYSDBA';
   connection.Params.Values['Password'] := 'masterkey';
   connection.Params.Values['Database'] := dbname;
   connection.Params.Values['ServerCharSet'] := 'UTF8';
   connection.Open;

   FDBName := dbname;

   //a little RTTI surgery so DBX won't quote all my table names
   cls := ctx.GetType(connection.MetaData.ClassType);
   cls.GetField('FQuotePrefix').SetValue(connection.MetaData, '');
   cls.GetField('FQuoteSuffix').SetValue(connection.MetaData, '');

   cls := ctx.GetType(connection.DBXConnection.ClassType);
   driver := cls.GetField('FDriverDelegate').GetValue(connection.DBXConnection).AsType<TDBXDriver>;

   cls := ctx.GetType(driver.classtype);
   driver := cls.GetField('FDriver').GetValue(driver).AsType<TDBXDynalinkDriver>;

   fld := ctx.GetType(TDBXDynalinkDriver).GetField('FMethodTable');
   table := fld.GetValue(driver).AsType<TDBXMethodTable>;
   table.FDBXWritableRow_SetBoolean := DBXWritableRow_SetBoolean;
   LSetSmallint := table.FDBXWritableRow_SetInt16;
end;

procedure TdmDatabase.BuildDatabase(const dbname: string; dbObj: TRpgDatafile);
const VERSION = 'INSERT INTO DB_VERSION (id, name) values (%d, %s);';
var
   list: TStringList;
   extracted: string;
   dataset: TDataset;
begin
   OpenConnection(dbname);
   list := TStringList.Create;
   try
      list.Text := self.GetDBScript;
      list.Add(format(VERSION, [dbObj.id, QuotedStr(dbObj.name)]));
      while list.count > 0 do
      begin
         ArbitraryQuery.Active := false;
         ArbitraryQuery.Sql.Clear;
         repeat
            extracted := list[0];
            list.Delete(0);
            ArbitraryQuery.SQL.Add(extracted);
         until extracted[length(extracted)] = ';';
         ArbitraryQuery.ExecSQL(true);
      end;
   finally
      list.free;
   end;
   Connect(dbname, nil);
   TMonitor.Enter(tilegroups);
   try
      for dataset in FAllDatasetList do
         dataset.Active := true;
   finally
      TMonitor.Exit(tilegroups);
   end;
end;

procedure TdmDatabase.charClasses_skillsetCalcFields(DataSet: TDataSet);
var
   func: TSkillGainDisplayFunc;
   result: string;
   args: T4IntArray;
   i: integer;
begin
func := nil; //TODO: Fix this
//   func := TSkillGainDisplayFunc(dataset.FieldByName('method_displayAddress').asPSMethod);
   if not assigned(TMethod(func).data) then
   begin
      result := format('Lv. %d', [dataset.fieldByName('nums_1').AsInteger]);
      dataset.FieldByName('name').AsString := self.nameLookup('skills', dataset.fieldByName('skill').AsInteger);
   end
   else begin
      for I := 1 to 4 do
         args[i] := dataset.FieldByName(format('nums_%d', [i])).AsInteger;
      if dataset.FieldByName('method_arrayArgs').AsBoolean then
         result := TSkillGainDisplayArrayFunc(func)(args)
      else
         result := func(args[1], args[2], args[3], args[4])
      //end if
   end;
   dataset.FieldByName('id').AsString := result;
end;

procedure TdmDatabase.classFilter(DataSet: TDataSet; var Accept: Boolean);
begin
   accept := Self.usableByFilter(DataSet.FieldByName('usableByClass') as TBlobField, charClasses);
end;

procedure TdmDatabase.restoreClone(DataSet: TDataSet);
begin
   if dataset.tag <> 0 then
   begin
      DataSet.AfterOpen := nil;
      (dataSet as TClientDataSet).CloneCursor(TClientDataset(dataset.Tag), false, true);
      DataSet.AfterOpen := self.restoreClone;
   end;
   TEBObject.Datastore := self;
end;

procedure TdmDatabase.SwitchesVarsCalcFields(DataSet: TDataSet);
const
   DISPLAY_NAME = '%.4d: %s';
var
   idField: TIntegerField;
   nameField: TWideStringField;
begin
   idField := dataset.FieldByName('id') as TIntegerField;
   nameField := dataset.FieldByName('name') as TWideStringField;
   dataset.FieldByName('DisplayName').AsString := format(DISPLAY_NAME, [idField.Value, nameField.Value]);
end;

function TdmDatabase.usableByFilter(field: TBlobField; master: TDataset): boolean;
begin
   result := master.FieldByName('id').AsInteger in field.asSet;
end;

{ TDatasetList }

function TDatasetList.FindByName(const name: string): TCustomClientDataset;
var
  L, H: Integer;
  mid, cmp: Integer;
begin
  Result := nil;
  L := 0;
  H := Count - 1;
  while L <= H do
  begin
    mid := L + (H - L) shr 1;
    cmp := StrIComp(PChar(self[mid].name), PChar(name));
    if cmp < 0 then
      L := mid + 1
    else
    begin
      H := mid - 1;
      if cmp = 0 then
        Exit(self[mid])
    end;
  end;
end;

end.
