unit db_upgrade;

interface
uses
   dm_database;

procedure UpgradeDatabase(database: TdmDatabase; const filename: string);

implementation
uses
   DB, SimpleDS, Generics.collections, SysUtils, IOUtils, Classes,
   EventBuilder, EB_Maps;

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
   database.SaveAll;
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

procedure Update42(database: TdmDatabase; const path: string);
begin
   ScanEvents(database, TEBFlashScreen, UpdateFlashScreen, path);
end;

initialization
   table := TDictionary<integer, TDBUpgradeProc>.Create;
   table.Add(42, Update42);
finalization
   table.Free;
end.
