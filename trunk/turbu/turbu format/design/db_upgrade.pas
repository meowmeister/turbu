unit db_upgrade;

interface
uses
   dm_database;

procedure UpgradeDatabase(database: TdmDatabase; const filename: string);

implementation
uses
   DB, SimpleDS, DBXCommon, Generics.collections, SysUtils, IOUtils, Classes,
   EventBuilder, EB_Maps, EB_Expressions;

type
   TDBUpgradeProc = procedure(database: TdmDatabase; const path: string);
var
   table: TDictionary<integer, TDBUpgradeProc>;

function UpgradeStep(database: TdmDatabase; const path: string): boolean;
var
   ds: TDataset;
   version: integer;
   proc: TDBUpgradeProc;
begin
   ds := database.dbData;
   TSimpleDataset(ds).DataSet.CommandText := UpperCase(TSimpleDataset(ds).DataSet.CommandText);
   ds.Active := true;
   version := ds.FieldByName('id').AsInteger;
   if table.TryGetValue(version, proc) then
   begin
      proc(database, path);
      ds.Edit;
      ds.FieldByName('id').AsInteger := version + 1;
      ds.Post;
      result := true;
      database.SaveAll;
   end
   else result := false;
end;

procedure UpgradeDatabase(database: TdmDatabase; const filename: string);
var
   path: string;
begin
   path := ExtractFilePath(filename);
   repeat
   until UpgradeStep(database, path) = false;
end;

type
   TEBUpdateProc = procedure (var obj: TEBObject);

procedure rScan(obj, parent: TEBObject; cls: TEBObjectClass; handler: TEBUpdateProc);
var
   old, child: TEBObject;
   idx: integer;
begin
   if obj.ClassType = cls then
   begin
      old := obj;
      handler(obj);
      if old <> obj then
      begin
         idx := parent.children.IndexOf(old);
         parent.children.Extract(old);
         parent.Children.Add(obj);
         parent.children.Extract(obj);
         parent.children.Insert(idx, obj);
         old.Free;
      end;
   end;
   for child in obj do
      rScan(child, obj, cls, handler);
end;

procedure InternalScanEvents(cls: TEBObjectClass; handler: TEBUpdateProc; const filename: string);
var
   stream: TStream;
   obj: TEBObject;
   script: utf8String;
begin
    stream := TFile.OpenRead(filename);
    obj := nil;
    try
       obj := TEBObject.LoadFromStream(stream);
       rScan(obj, nil, cls, handler);
       script := utf8String(obj.Serialize);
       stream.Free;
       stream := TFile.Open(filename, TFileMode.fmCreate);
       stream.write(script[1], length(script));
    finally
       stream.Free;
       obj.Free;
    end;
end;

procedure InternalScanField(cls: TEBObjectClass; handler: TEBUpdateProc; field: TWideMemoField);
var
   stream: TStream;
   obj: TEBObject;
begin
    stream := TStringStream.Create(field.AsString);
    obj := nil;
    try
       obj := TEBObject.LoadFromStream(stream);
       rScan(obj, nil, cls, handler);
       field.DataSet.Edit;
       field.AsString := obj.Serialize;
       field.DataSet.Post;
    finally
       stream.Free;
       obj.Free;
    end;
end;

procedure ScanEvents(database: TdmDatabase; cls: TEBObjectClass; handler: TEBUpdateProc; const path: string);
var
   name, filename: string;
   list: TStringList;
   events: TSimpleDataset;
   field: TWideMemoField;
begin
   name := cls.ClassName;
   list := TStringList.Create;
   try
      for filename in TDirectory.GetFiles(TPath.Combine(path, 'scripts')) do
         if pos(name, TFile.ReadAllText(filename)) > 0 then
            list.add(filename);
      for filename in list do
         InternalScanEvents(cls, handler, filename);

      events := database.mparties_events as TSimpleDataset;
      events.DataSet.CommandText := UpperCase(events.DataSet.CommandText);
      events.Active := true;
      field := events.FieldByName('eventText') as TWideMemoField;
      events.First;
      while not events.Eof do
      begin
         if pos(name, field.AsString) <> 0 then
            InternalScanField(cls, handler, field);
         events.Next;
      end;
   finally
      list.free;
   end;
end;

procedure UpdateFlashScreen(var obj: TEBObject);
begin
   assert(obj.ClassType = TEBFlashScreen);
   if obj.Values.Count < 7 then
      obj.Values.Add(0)
   else if obj.Values[6] = 2 then
      obj := TEBEndFlash.Create(nil);
end;

procedure UpdateShakeScreen(var obj: TEBObject);
begin
   assert(obj.ClassType = TEBShakeScreen);
   if obj.Values.Count < 5 then
      obj.Values.Add(0)
   else if obj.Values[4] = 2 then
      obj := TEBEndShake.Create(nil);
end;

procedure UpdateArraySkillsName(var obj: TEBObject);
begin
   assert(obj.ClassType = TEBObjArrayValue);
   if obj.text = 'skills' then
      obj.Text := 'skill';
end;

procedure Update42(database: TdmDatabase; const path: string);
begin
   ScanEvents(database, TEBFlashScreen, UpdateFlashScreen, path);
   ScanEvents(database, TEBShakeScreen, UpdateShakeScreen, path);
end;

procedure Update43(database: TdmDatabase; const path: string);
begin
   ScanEvents(database, TEBObjArrayValue, UpdateArraySkillsName, path);
end;

function Update44id(const maps, filename: string): integer;
var
   baseFilename: string;
   stream: TStream;
   id: word;
begin
   if filename = 'globalevents' then
      Exit(0);
   baseFilename := TPath.Combine(maps, filename)+ '.tmf';
   if not FileExists(baseFilename) then
      Exit(-1);

   stream := TFile.OpenRead(baseFilename);
   stream.Read(id, sizeof(word));
   stream.Free;
   result := id;
end;

procedure Update44(database: TdmDatabase; const path: string);
const
   SQL = 'CREATE TABLE SCRIPT_CACHE (ID INTEGER NOT NULL, SCRIPT BLOB SUB_TYPE TEXT NOT NULL)';
   SQL2 = 'ALTER TABLE SCRIPT_CACHE ADD CONSTRAINT PK_SCRIPT_CACHE PRIMARY KEY (ID)';
   SQL3 = 'DROP TABLE SCRIPT_CACHE';
var
   filename, maps: string;
   id: integer;
   obj: TEBObject;
   tran: TdbxTransaction;
begin
   tran := database.Connection.BeginTransaction;
   database.connection.ExecuteDirect(SQL);
   try
      database.connection.ExecuteDirect(SQL2);
      database.Connection.CommitFreeAndNil(tran);
      database.script_cache.DataSet.CommandText := UpperCase(database.script_cache.DataSet.CommandText);
      database.script_cache.Active := true;
      maps := TPath.Combine(path, 'maps');
      for filename in TDirectory.GetFiles(TPath.Combine(path, 'scripts')) do
      begin
         id := Update44id(maps, TPath.GetFileNameWithoutExtension(filename));
         if id = -1 then
            Continue;
         obj := TEBObject.Load(TFile.ReadAllText(filename));
         try
            database.script_cache.append;
            try
               database.script_cache.FieldByName('id').AsInteger := id;
               database.script_cache.FieldByName('script').Asstring := obj.GetScript(0);
               database.script_cache.Post;
            except
               database.script_cache.cancel;
            end;
         finally
            obj.Free;
         end;
      end;
   except
      database.connection.ExecuteDirect(SQL3);
      raise;
   end;
end;

initialization
   table := TDictionary<integer, TDBUpgradeProc>.Create;
   table.Add(42, Update42);
   table.Add(43, Update43);
   table.Add(44, Update44);
finalization
   table.Free;
end.
